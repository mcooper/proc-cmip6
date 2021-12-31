#Elevation
cd ~/maldat/covars/0.25dd/historic
find . -mindepth 2 -type d -exec ln -sf /home/mattcoop/maldat/covars/0.25dd/elevation.tif '{}/elevation.tif' \;
cd ~/maldat/covars/0.05dd/historic
find . -mindepth 2 -type d -exec ln -sf /home/mattcoop/maldat/covars/0.25dd/elevation.tif '{}/elevation.tif' \;
cd ~/maldat/covars/0.25dd
find . -mindepth 4 -type d -exec ln -sf /home/mattcoop/maldat/covars/0.25dd/elevation.tif '{}/elevation.tif' \;

#esh, lifexp, msch, and gni_percap
cd ~/maldat/covars/0.25dd/historic
for f in $(find "$(pwd)" -maxdepth 2 -mindepth 2 -type d)
do
  cd $f
  echo $f
  ln -sf ../esch.tif $f/esch.tif
  ln -sf ../msch.tif $f/msch.tif
  ln -sf ../lifexp.tif $f/lifexp.tif
  ln -sf ../gni_percap.tif $f/gni_percap.tif
done

cd ~/maldat/covars/0.05dd/historic
for f in $(find "$(pwd)" -maxdepth 2 -mindepth 2 -type d)
do
  cd $f
  echo $f
  ln -sf ../esch.tif $f/esch.tif
  ln -sf ../msch.tif $f/msch.tif
  ln -sf ../lifexp.tif $f/lifexp.tif
  ln -sf ../gni_percap.tif $f/gni_percap.tif
done

cd ~/maldat/covars/0.25dd/
for f in $(find "$(pwd)" -mindepth 4 -type d)
do
  cd $f
  echo $f
  ln -sf ../../esch.tif $f/esch.tif
  ln -sf ../../msch.tif $f/msch.tif
  ln -sf ../../lifexp.tif $f/lifexp.tif
  ln -sf ../../gni_percap.tif $f/gni_percap.tif
done
