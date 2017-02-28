#!/bin/bash

R_HOME="${OPENSHIFT_DATA_DIR}r"
R_VERSION="3.3.0"

mkdir -p $R_HOME

cd $R_HOME

if [ -e /sandbox/r ]; then
  unlink /sandbox/r
fi

ln -s `pwd` /sandbox/r

if [ -e "${OPENSHIFT_REPO_DIR}node_modules/.bin/R" ]; then
  unlink ${OPENSHIFT_REPO_DIR}node_modules/.bin/R
fi

ln "${OPENSHIFT_DATA_DIR}r/bin/R" -s "${OPENSHIFT_REPO_DIR}node_modules/.bin/R"

if [ ! -f $R_HOME/bin/R ]; then

  echo "No installation found:"

  if [ ! -e "R-${R_VERSION}.tar.gz" ]; then
    echo -ne "Downloading R sources ..\r"
    echo "downloading" > $OPENSHIFT_LOG_DIR/installR.log 2>&1
    wget --quiet http://cran.cnr.berkeley.edu/src/base/R-3/R-${R_VERSION}.tar.gz >> $OPENSHIFT_LOG_DIR/installR.log 2>&1
    echo -ne "Downloading R sources ... done\r"
    echo ''
  fi 

  if [ -e "R-${R_VERSION}" ]; then
    rm -rf "R-${R_VERSION}"
  fi

  tar -xf R-${R_VERSION}.tar.gz

wget http://zlib.net/zlib-1.2.8.tar.gz
tar -xf zlib-1.2.8.tar.gz
cd zlib-1.2.8
./configure --prefix=/sandbox/zlib
make
make install

cd ..
wget http://www.bzip.org/1.0.6/bzip2-1.0.6.tar.gz
tar -xf bzip2-1.0.6.tar.gz
cd bzip2-1.0.6
make
make install PREFIX=/sandbox/bzip2

cd ..
wget http://tukaani.org/xz/xz-5.2.2.tar.gz
tar -xf xz-5.2.2.tar.gz
cd xz-5.2.2
./configure --prefix=/sandbox/lzma
make
make install

cd ..
wget ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.38.tar.bz2
tar -xf pcre-8.38.tar.bz2
cd pcre-8.38
./configure --prefix=/sandbox/pcre --enable-utf8 --enable-unicode-properties
make
make install
cd /sandbox/pcre/include
mkdir pcre
cp * pcre

cd
cd app-root
cd repo
cd R
wget https://curl.haxx.se/download/curl-7.49.1.tar.bz2
tar -xf curl-7.49.1.tar.bz2
cd curl-7.49.1
./configure --prefix=/sandbox/curl
make
make install

cd..
wget http://www.mega-nerd.com/libsndfile/files/libsndfile-1.0.26.tar.gz
tar -xf libsndfile-1.0.26.tar.gz
cd libsndfile-1.0.26
./configure --prefix=/sandbox/libsndfile
make
make install

cd..
wget http://www.fftw.org/fftw-3.3.4.tar.gz
tar -xf fftw-3.3.4.tar.gz
cd fftw-3.3.4
./configure --prefix=/sandbox/fftw --with-pic
make
make install
export PKG_CONFIG_PATH=/sandbox/fftw/lib/pkgconfig

cd ..
cd "R-${R_VERSION}"

export LDFLAGS="-L/sandbox/zlib/lib -L/sandbox/bzip2hc/lib -L/sandbox/lzma/lib -L/sandbox/pcre/lib -L/sandbox/curl/lib -L/sandbox/libsndfile/lib -L/sandbox/fftw/lib"
export CPPFLAGS="-I/sandbox/zlib/include -I/sandbox/bzip2hc/include -I/sandbox/lzma/include -I/sandbox/pcre/include -I/sandbox/curl/include -I/sandbox/libsndfile/include -I/sandbox/fftw/include"

# Note, anytime you change th LDFLAGS or CPPFLAGS path, you need to run the ./configure command below and make install (to update R).
./configure --prefix=/sandbox/r --with-recommended-packages=no
make
make install

  #cd "R-${R_VERSION}"
  #echo -ne "configuring R ..\r"
  #./configure --prefix=/sandbox/r --with-recommended-packages=no >> $OPENSHIFT_LOG_DIR/installR.log 2>&1
  #echo -ne "configuring R ... done\r"
  #echo ''
  #echo -ne "building R ..\r"
  #make --silent >> $OPENSHIFT_LOG_DIR/installR.log 2>&1
  #echo -ne "building R ... done\r"
  #echo ''
  #echo -ne "installing R ..\r"
  #make --silent install >> $OPENSHIFT_LOG_DIR/installR.log 2>&1
  #echo -ne "installing R ... done\r"
  #rm -rf "R-${R_VERSION}"
  #echo ''
  
else
  echo "R already installed in the gear"
fi

cd ..
