SHARE=/usr/local/spk/share
ARCH=i686
MODE=test
mkdir $SHARE/working/spk$MODE/spkjob-$1
$SHARE/arch/$ARCH/bin/spk$MODE/spkjob $1 $2 $MODE


