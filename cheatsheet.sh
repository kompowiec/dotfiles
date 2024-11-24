for file in *.rar; do; mv -- "$file" "${file%.rar}.cbr"; done # To change all files in a directory from .rar to .cbr
sudo pacman-key --refresh-keys #(Niepoprawny lub uszkodzony pakiet (podpis PGP)).

#yay
sudo mkdir -p /data/home/username/.cache/go-build
sudo chown -R username:username /data/home/username/.cache/go-build
go build -trimpath -mod=readonly -modcacherw -ldflags '-X "main.yayVersion=12.3.5" -X "main.localePath=/usr/share/locale/" -linkmode=external -compressdwarf=false' -buildmode=pie -o yay -buildvcs=false
go mod init
go mod init example.com/yay
sudo chown -R username:username /home/username/yay/pkg



