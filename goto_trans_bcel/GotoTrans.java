import java.util.Iterator;
import java.util.Hashtable;
import java.io.File;
import java.util.logging.*;

import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;
import org.apache.bcel.Repository;
import org.apache.bcel.util.InstructionFinder;

public class GotoTrans {
  private static Logger logger = LogManager.getLogManager().getLogger("");

  private static void prepare_logger() {
    class TrivialFormatter extends Formatter {
      public String format(LogRecord record) {
        return record.getLevel() + ": " + record.getMessage() + "\n";
      }
    }

    StreamHandler sh = new StreamHandler(System.err, new TrivialFormatter());
    Handler[] handlers = logger.getHandlers();
    for(int i=0;i<handlers.length;i++)
      logger.removeHandler(handlers[i]);
    logger.addHandler(sh);
    logger.setLevel(Level.ALL);
  }

  public static void main(String args[]) {
    JavaClass clazz=null;
    Method [] methods;
    ConstantPoolGen cp;
    String myClassName;
    int cidx;

    prepare_logger();

    for(int i=0;i<args.length;i++) {

      cidx = args[i].lastIndexOf(".class");
      myClassName = (cidx >=0) ? args[i].substring(0,cidx) : args[i];

      logger.info("Processing " + myClassName);

      try {
        clazz = Repository.lookupClass(myClassName);
      } catch (Exception e) {
        System.err.println("Error locating " + myClassName + ": " + e.getMessage());
        System.exit(1); 
      }

      methods = clazz.getMethods();
      cp = new ConstantPoolGen(clazz.getConstantPool());

      for(int j=0;j<methods.length;j++) {
        if(!(methods[j].isAbstract() || methods[j].isNative())) {
          //System.out.println(methods[j]);
          MethodGen mg = new MethodGen(methods[j], clazz.getClassName(), cp);

          Hashtable<Integer,InstructionHandle> labelHash = new Hashtable<Integer,InstructionHandle>();
          InstructionList   il    = mg.getInstructionList();

          int lcount = findLabels(il,cp,labelHash);

//System.out.println("labelHash:");
//System.out.println(labelHash);

          int bcount = findBranches(il,cp,labelHash);

          if(lcount + bcount > 0)
            methods[j] = mg.getMethod();
        }
      }
      clazz.setConstantPool(cp.getFinalConstantPool());
      try {
        File origfile = new File(myClassName + ".class");
        File bakfile = new File(myClassName + ".class.old");

        if(!origfile.renameTo(bakfile))
          System.err.println("Warning: failed to create backup file " + bakfile);

        clazz.dump(myClassName + ".class");
      } catch(java.io.IOException e) {
        System.err.println("Failed to dump class: " + myClassName);
      }
    }
  }

  public static int findLabels(InstructionList il, ConstantPoolGen cp, Hashtable<Integer,InstructionHandle> h) {
    InstructionFinder f     = new InstructionFinder(il);
    String            pat   = "(LDC|LDC_W) (LDC|LDC_W|BIPUSH|SIPUSH|ICONST) INVOKESTATIC";
    InstructionHandle next  = null;
    int               i,j,count = 0;

    for(Iterator iter = f.search(pat); iter.hasNext(); ) {
      InstructionHandle[] match = (InstructionHandle[])iter.next();

      if(match.length >= 3) {
        //for(i=0;i<match.length;i++) {
        //  System.out.println("instruction " + i + " = " + match[i]);
        //}

        Instruction instr = match[2].getInstruction();

        if(instr instanceof INVOKESTATIC) {
          INVOKESTATIC inv_instr = (INVOKESTATIC) instr;

          next = match[2].getNext();

          if(inv_instr.getMethodName(cp).equals("label") && 
             inv_instr.getLoadClassType(cp).getClassName().equals("org.netlib.util.Dummy"))
          {
            Instruction load_instr = match[1].getInstruction();
            int label=-1;

            if(load_instr instanceof LDC)
              label = ((Number)(((LDC)load_instr).getValue(cp))).intValue();
            else if(load_instr instanceof BIPUSH)
              label = ((BIPUSH)load_instr).getValue().intValue();
            else if(load_instr instanceof SIPUSH)
              label = ((SIPUSH)load_instr).getValue().intValue();
            else if(load_instr instanceof ICONST)
              label = ((ICONST)load_instr).getValue().intValue();
            else
              System.err.println("Error: Could not determine label number");

            if(label >= 0) {
              System.out.println("found label: " + label);

//
              InstructionTargeter[] targeters = match[0].getTargeters();
              InstructionHandle new_nop = il.insert(match[0], new NOP());

              for(j=0; j < targeters.length; j++)
                targeters[j].updateTarget(match[0], new_nop);
//

              try {
                il.delete(match[0],match[2]);
              } catch(TargetLostException e) {
                InstructionHandle[] targets = e.getTargets();
System.out.println("handling lost targets..." + targets.length);
                for(i=0; i < targets.length; i++) {
                  targeters = targets[i].getTargeters();
                  //InstructionTargeter[] targeters = targets[i].getTargeters();
System.out.println("   num targeters = " + targeters.length);

                  for(j=0; j < targeters.length; j++)
                    targeters[j].updateTarget(targets[i], new_nop);
                    //targeters[j].updateTarget(targets[i], next);
//System.out.println("   set target = " + next);
System.out.println("   set target = " + new_nop);
                }
              }

              count++;

              //h.put(new Integer(label), next);
              h.put(new Integer(label), new_nop);
            }
          }
        }
      }
    }

    return count;
  }

  public static int findBranches(InstructionList il, ConstantPoolGen cp, Hashtable<Integer,InstructionHandle> h) {
    InstructionFinder f     = new InstructionFinder(il);
    String            pat   = "(LDC|LDC_W) (LDC|LDC_W|BIPUSH|SIPUSH|ICONST) INVOKESTATIC";
    InstructionHandle next  = null;
    InstructionHandle target = null;
    int i,j,count = 0;

    for(Iterator iter = f.search(pat); iter.hasNext(); ) {
      InstructionHandle[] match = (InstructionHandle[])iter.next();

      if(match.length >= 3) {
        //for(i=0;i<match.length;i++) {
        //  System.out.println("instruction " + i + " = " + match[i]);
        //}

        Instruction instr = match[2].getInstruction();

        if(instr instanceof INVOKESTATIC) {
          INVOKESTATIC inv_instr = (INVOKESTATIC) instr;

          next = match[2].getNext();

          if(inv_instr.getMethodName(cp).equals("go_to") && 
             inv_instr.getLoadClassType(cp).getClassName().equals("org.netlib.util.Dummy"))
          {
            Instruction load_instr = match[1].getInstruction();
            int label=-1;

            if(load_instr instanceof LDC)
              label = ((Number)(((LDC)load_instr).getValue(cp))).intValue();
            else if(load_instr instanceof BIPUSH)
              label = ((BIPUSH)load_instr).getValue().intValue();
            else if(load_instr instanceof SIPUSH)
              label = ((SIPUSH)load_instr).getValue().intValue();
            else if(load_instr instanceof ICONST)
              label = ((ICONST)load_instr).getValue().intValue();
            else
              System.err.println("Error: Could not determine label number");

            if(label >= 0) {
              //System.out.println("found branch to: " + label);

              InstructionTargeter[] targeters = match[0].getTargeters();

              target = h.get(new Integer(label));
//System.out.println("branch to: " + label + " = " + target);
              if(target != null) {
                InstructionHandle new_branch = il.insert(match[0], new GOTO_W(target));

                for(j=0; j < targeters.length; j++)
                  targeters[j].updateTarget(match[0], new_branch);
              }
              else {
                System.err.println("Warning: branch target " + label + " not found.");
              }

              try {
                il.delete(match[0],match[2]);
              } catch(TargetLostException e) {
                InstructionHandle[] targets = e.getTargets();
                for(i=0; i < targets.length; i++) {
                  targeters = targets[i].getTargeters();

                  for(j=0; j < targeters.length; j++)
                    targeters[j].updateTarget(targets[i], next);
                }
              }

              count++;
            }
          }
        }
      }
    }

    return count;
  }
}
