cd ~/maldat/raw/chirps/

for f in $(ls)
do
  echo $f
  gdalwarp -te -20 -35 55 25 $f afr/$f
done

# temps
cd ~/maldat/raw/cru_ts_4/

for f in $(ls)
do
  echo $f
  gdalwarp -te -20 -35 55 25 $f afr/$f
done
