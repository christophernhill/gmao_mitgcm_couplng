#! /bin/csh -f

foreach i (*_checkpoint)
  set j=$i:s/_checkpoint/_rst/
  rm $j
  mv $i $j
end
