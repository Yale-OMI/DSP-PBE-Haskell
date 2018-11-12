# Clean up old ramdisk if for some reason we didnt before
sudo umount ./tmp
rm -rf ./tmp

# Create new ramdisk - this frees most of the IO
# bottleneck. All the temp files are written to RAM and
# never committed to disk. 
mkdir -p ./tmp
sudo mount -t tmpfs -o size=256m tmpfs ./tmp/

time cabal new-run pldi_benchmarks -- --color=always


