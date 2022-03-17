#!/bin/bash
set -e -x
cobc -x GARCIA-P04-MAILING-LABELS.cob
./GARCIA-P04-MAILING-LABELS
