
/*****************************************************************************
 * class.c                                                                   *
 *                                                                           *
 * This file contains routines for writing the class file structure to disk. *
 *                                                                           *
 *****************************************************************************/

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include"class.h"
#include"constant_pool.h"
#include"f2jparse.tab.h"

void
write_class(struct ClassFile *class)
{
  FILE *cfp;

  cfp = open_output_classfile(class);

  fwrite(&(class->magic), sizeof(class->magic), 1, cfp);
  fwrite(&(class->minor_version), sizeof(class->minor_version), 1, cfp);
  fwrite(&(class->major_version), sizeof(class->major_version), 1, cfp);
  fwrite(&(class->constant_pool_count), sizeof(class->constant_pool_count), 1, cfp);
  write_constant_pool(class, cfp);
  fwrite(&(class->access_flags), sizeof(class->access_flags), 1, cfp);
  fwrite(&(class->this_class), sizeof(class->this_class), 1, cfp);
  fwrite(&(class->super_class), sizeof(class->super_class), 1, cfp);
  fwrite(&(class->interfaces_count), sizeof(class->interfaces_count), 1, cfp);
  write_interfaces(class,cfp);
  fwrite(&(class->fields_count), sizeof(class->fields_count), 1, cfp);
  write_fields(class,cfp);
  fwrite(&(class->methods_count), sizeof(class->methods_count), 1, cfp);
  write_methods(class,cfp);
  fwrite(&(class->attributes_count), sizeof(class->attributes_count), 1, cfp);
  write_attributes(class->attributes,class->constant_pool,cfp);

  fclose(cfp);
}

/*****************************************************************************
 * write_constant_pool                                                       *
 *                                                                           *
 * This function writes the all the constants to disk.  this could be more   *
 * efficient if we could assume that there was no padding in the structures. *
 * then it would just be a matter of writing out however many bytes is       *
 * allocated.  but i'm not really sure how different compilers might pad     *
 * structures, so i'm going to play it safe here and just write each item    *
 * individually.  --kgs 4/25/00                                              *
 *                                                                           *
 *****************************************************************************/

void
write_constant_pool(struct ClassFile *class, FILE *out)
{
  CPNODE * tmpconst;
  Dlist tmpPtr;

  dl_traverse(tmpPtr,class->constant_pool) {
    tmpconst = (CPNODE *) tmpPtr->val;

    fwrite(&(tmpconst->val->tag),sizeof(tmpconst->val->tag),1,out);

    switch(tmpconst->val->tag) {
      case CONSTANT_Utf8:
        fwrite(&(tmpconst->val->cpnode.Utf8.length),
           sizeof(tmpconst->val->cpnode.Utf8.length),1,out);
        fwrite(tmpconst->val->cpnode.Utf8.bytes,
           tmpconst->val->cpnode.Utf8.length,1,out);
        break;
      case CONSTANT_Integer:
        fwrite(&(tmpconst->val->cpnode.Integer.bytes),
           sizeof(tmpconst->val->cpnode.Integer.bytes),1,out);
        break;
      case CONSTANT_Float:
        fwrite(&(tmpconst->val->cpnode.Float.bytes),
           sizeof(tmpconst->val->cpnode.Float.bytes),1,out);
        break;
      case CONSTANT_Long:
        fwrite(&(tmpconst->val->cpnode.Long.high_bytes),
           sizeof(tmpconst->val->cpnode.Long.high_bytes),1,out);
        fwrite(&(tmpconst->val->cpnode.Long.low_bytes),
           sizeof(tmpconst->val->cpnode.Long.low_bytes),1,out);
        break;
      case CONSTANT_Double:
        fwrite(&(tmpconst->val->cpnode.Double.high_bytes),
           sizeof(tmpconst->val->cpnode.Double.high_bytes),1,out);
        fwrite(&(tmpconst->val->cpnode.Double.low_bytes),
           sizeof(tmpconst->val->cpnode.Double.low_bytes),1,out);
        break;
      case CONSTANT_Class:
        fwrite(&(tmpconst->val->cpnode.Class.name_index),
           sizeof(tmpconst->val->cpnode.Class.name_index),1,out);
        break;
      case CONSTANT_String:
        fwrite(&(tmpconst->val->cpnode.String.string_index),
           sizeof(tmpconst->val->cpnode.String.string_index),1,out);
        break;
      case CONSTANT_Fieldref:
      case CONSTANT_Methodref:
      case CONSTANT_InterfaceMethodref:
        fwrite(&(tmpconst->val->cpnode.Methodref.class_index),
           sizeof(tmpconst->val->cpnode.Methodref.class_index),1,out);
        fwrite(&(tmpconst->val->cpnode.Methodref.name_and_type_index),
           sizeof(tmpconst->val->cpnode.Methodref.name_and_type_index),1,out);
        break;
      case CONSTANT_NameAndType:
        fwrite(&(tmpconst->val->cpnode.NameAndType.name_index),
           sizeof(tmpconst->val->cpnode.NameAndType.name_index),1,out);
        fwrite(&(tmpconst->val->cpnode.NameAndType.descriptor_index),
           sizeof(tmpconst->val->cpnode.NameAndType.descriptor_index),1,out);
        break;
      default:
        fprintf(stderr,"WARNING: unknown tag in write_constant_pool()\n");
        break;  /* ANSI requirement */
    }
  }
}

/*****************************************************************************
 * write_interfaces                                                          *
 *                                                                           *
 * This function writes the all the interfaces to disk.                      *
 * Currently f2java generated classes do not implement any interfaces, so    *
 * this function will go unimplemented until we actually need it.            *
 *                                                                           *
 *****************************************************************************/

void
write_interfaces(struct ClassFile *class, FILE *out)
{
}

/*****************************************************************************
 * write_fields                                                              *
 *                                                                           *
 * This function writes the all the fields to disk.                          *
 *                                                                           *
 *****************************************************************************/

void
write_fields(struct ClassFile *class, FILE *out)
{
  struct field_info *tmpfield;
  Dlist tmpPtr;

  dl_traverse(tmpPtr,class->fields) {
    tmpfield = (struct field_info *) tmpPtr->val;

    fwrite(&(tmpfield->access_flags),sizeof(tmpfield->access_flags),1,out);
    fwrite(&(tmpfield->name_index),sizeof(tmpfield->name_index),1,out);
    fwrite(&(tmpfield->descriptor_index),sizeof(tmpfield->descriptor_index),1,out);

    /* we do not expect there to be any field attributes, so check the 
     * count and issue a warning message if count > 0
     */
  
    if(tmpfield->attributes_count > 0) {
      fprintf(stderr,"WARNING: not expecting attributes on a field!\n");
      tmpfield->attributes_count = 0;
    }

    fwrite(&(tmpfield->attributes_count),sizeof(tmpfield->attributes_count),1,out);

    /* here is where we'd write the attributes themselves, if f2j should
     * ever need to use them.
     *
     * write_field_attributes(tmpfield,out);
     */
  }
}

void
write_methods(struct ClassFile *class, FILE *out)
{
  struct method_info *tmpmeth;
  Dlist tmpPtr;

  dl_traverse(tmpPtr,class->methods) {
    tmpmeth = (struct method_info *) tmpPtr->val;

    fwrite(&(tmpmeth->access_flags),sizeof(tmpmeth->access_flags),1,out);
    fwrite(&(tmpmeth->name_index),sizeof(tmpmeth->name_index),1,out);
    fwrite(&(tmpmeth->descriptor_index),sizeof(tmpmeth->descriptor_index),1,out);
    fwrite(&(tmpmeth->attributes_count),sizeof(tmpmeth->attributes_count),1,out);

    write_attributes(tmpmeth->attributes,class->constant_pool,out);
  }
}

/*****************************************************************************
 * write_attributes                                                          *
 *                                                                           *
 * This function writes the all the attributes to disk.                      *
 * we dont need to support all attributes since f2j will only use a few.     *
 *                                                                           *
 *****************************************************************************/

void
write_attributes(Dlist attr_list, Dlist const_pool, FILE *out)
{
  struct attribute_info *tmpattr;
  char *attr_name;
  Dlist tmpPtr;
  CPNODE *c;

  if((attr_list == NULL) || (const_pool == NULL))
    return;

  dl_traverse(tmpPtr,attr_list) {
    tmpattr = (struct attribute_info *) tmpPtr->val;

    c = cp_entry_by_index(const_pool, tmpattr->attribute_name_index);
 
    if(c==NULL) {
      fprintf(stderr,"WARNING: write_attributes() can't find attribute name\n");
      continue;
    } 
      
    attr_name = null_term(c->val->cpnode.Utf8.bytes, c->val->cpnode.Utf8.length);

    fwrite(&(tmpattr->attribute_name_index),
      sizeof(tmpattr->attribute_name_index),1,out);
    fwrite(&(tmpattr->attribute_length),
      sizeof(tmpattr->attribute_length),1,out);

    if(!strcmp(attr_name,"SourceFile")) {
      fwrite(&(tmpattr->attr.SourceFile.sourcefile_index),
         sizeof(tmpattr->attr.SourceFile.sourcefile_index),1,out);
    } else {
      fprintf(stderr,"WARNING: write_attributes() unsupported attribute!\n");
    }
  }
}

FILE *
open_output_classfile(struct ClassFile *class)
{
  char *filename;
  FILE *newfp;
  CPNODE *c;
  
  if(class == NULL)
    return NULL;

  c = cp_entry_by_index(class->constant_pool, class->this_class);
  c = cp_entry_by_index(class->constant_pool, c->val->cpnode.Class.name_index);

  if(c==NULL) {
    fprintf(stderr,"Error opening class file.\n");
    exit(1);
  }

  /* malloc enough characters in the filename for:
   *  - the class name
   *  - plus 6 chars for ".class"
   *  - plus 1 char for the null terminator
   */

  filename = (char *)malloc(c->val->cpnode.Utf8.length +  7);
  strncpy(filename, (char *)c->val->cpnode.Utf8.bytes, c->val->cpnode.Utf8.length);
  filename[c->val->cpnode.Utf8.length] = '\0';
  strcat(filename,".class");

  if( (newfp = fopen(filename,"wb")) == NULL ) {
    fprintf(stderr,"Cannot open output file '%s'\n",filename);
    perror("Reason");
    exit(1);
  }

  return newfp;
}
