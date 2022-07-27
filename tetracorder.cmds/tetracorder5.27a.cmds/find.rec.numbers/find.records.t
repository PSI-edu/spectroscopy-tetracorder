#!/bin/sh

l4=/d1/speclib/speclib04/splib04t
l1b=/d1/sprlib/lib01/sprlb01t
l1n=/d1/sprlib/lib01/sprln01t
l1f=/d1/sprlib/lib01/sprlf01t

tmp=/tmp/tmp$$

spprint $l4  | sed -e 's/^/splib04t /'  > $tmp

spprint $l1b | sed -e 's/^/sprlb01t /' >> $tmp

spprint $l1n | sed -e 's/^/sprln01t /' >> $tmp

spprint $l1f | sed -e 's/^/sprlf01t /' >> $tmp

cat $tmp

rm $tmp
