#!/bin/sh

script_dir=`dirname $0`

java -classpath .:${script_dir}:${script_dir}/bcel-5.2.jar GotoTrans $*
