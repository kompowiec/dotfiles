for file in *.rar; do; mv -- "$file" "${file%.rar}.cbr"; done # To change all files in a directory from .rar to .cbr
sudo pacman-key --refresh-keys #(Niepoprawny lub uszkodzony pakiet (podpis PGP)).

#yay
sudo mkdir -p /data/home/username/.cache/go-build
sudo chown -R username:username /data/home/username/.cache/go-build
go build -trimpath -mod=readonly -modcacherw -ldflags '-X "main.yayVersion=12.3.5" -X "main.localePath=/usr/share/locale/" -linkmode=external -compressdwarf=false' -buildmode=pie -o yay -buildvcs=false
go mod init
go mod init example.com/yay
sudo chown -R username:username /home/username/yay/pkg
find . -type f -name '* (1).*' -exec rm -v {} \; #To remove files with (1) in their names on Linux, you can use the find command with rm. Here is the exact command you can use
# To remove files with `(1)` in their names on Linux, you can use the `find` command with `rm`. Here is the exact command you can use:
# Explanation:
#- `find .`: Searches in the current directory.
#- `-type f`: Finds files only.
#- `-name '* (1).*'`: Matches files with `(1)` before the extension.
#- `-exec rm -v {} \;`: Deletes each matching file, showing the names of deleted files.
#**Note**: Make sure to run this command from the directory containing the files or specify the correct path to the directory. Always double-check files before deleting to avoid accidental data loss.



