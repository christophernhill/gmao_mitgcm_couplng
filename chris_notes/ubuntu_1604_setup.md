AWS ami
```
ubuntu/images/hvm-ssd/ubuntu-xenial-16.04-amd64-server-20160815 (ami-c60b90d1)
c4.8xlarge
```

login
```
ssh -i ~/.ssh/XXXMYKEYXXX -l ubuntu IPADDRESS
```

setup
```
apt install gcc
apt install gfortran-5
apt-get install gfortran
apt-get install openmpi-'*'
apt-get install libopenmpi-'*'
```


Some possible commands for gfortran-6
```
add-apt-repository ppa:ubuntu-toolchain-r/test
apt-get update
apt-get install gfortran-6
```

