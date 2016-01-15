#!/usr/bin/env bash

HJC="./hjc -v false"

TARBALL=$1
MILESTONE=$2
mkdir -p log
LOG=log/$MILESTONE-`date +%F-%T`.log
source "$(dirname $0)/hackojo-login.sh"

$HJC exercise_focus flap
$HJC answers_upload --on flap flap.tar.gz $TARBALL
$HJC exercise_answer --on flap $MILESTONE file:flap.tar.gz
echo -n 'Waiting for answers...'
touch $LOG
while [ `wc -l $LOG | cut -f1 -d' '` -le 10 ]; do
  sleep 3
  $HJC exercise_evaluation_state --on flap $MILESTONE | tee -a $LOG
done
echo $LOG written and committed.
git add $LOG
git commit -a -m "Submission $LOG"
