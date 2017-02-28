#!/bin/bash

R_HOME="${OPENSHIFT_DATA_DIR}r"
R_VERSION="3.3.0"


mkdir -p $R_HOME

cd $R_HOME

if [ -e /sandbox/r ]; then
  unlink /sandbox/r
fi

ln -s `pwd` /sandbox/r

if [ ! -d $R_HOME/lib64/R/library/Rcpp ]; then
  echo -ne "installing Rcpp ..\r"
  wget --quiet http://cran.es.r-project.org/src/contrib/Rcpp_0.12.5.tar.gz > $OPENSHIFT_LOG_DIR/installRcpp.log 2>&1
  bin/R CMD INSTALL Rcpp_0.12.5.tar.gz >> $OPENSHIFT_LOG_DIR/installRcpp.log 2>&1
  echo -ne "installing Rcpp ... done\r"
  echo ''
  rm Rcpp_0.12.5.tar.gz
else
  if [ -f Rcpp_0.12.5.tar.gz ]; then
    echo -ne "reinstalling Rccp ..\r"
    bin/R CMD INSTALL Rcpp_0.12.5.tar.gz >> $OPENSHIFT_LOG_DIR/installRcpp.log 2>&1
    echo -ne "reinstalling Rccp ...done\r"
    rm Rcpp_0.12.5.tar.gz
  else
    echo -ne "Rcpp already installed"
  fi
  echo ''
fi

if [ ! -d $R_HOME/lib64/R/library/devtools ]; then
  echo -ne "installing devtools ..\r"
  bin/Rscript -e "install.packages('devtools',contriburl = contrib.url('http://cran.r-project.org'))" > $OPENSHIFT_LOG_DIR/installRdevtools.log 2>&1
  echo -ne "installing devtools ... done\r"
  echo ''
else
  echo -ne "devtools already installed\r"
  echo ''
fi

wget https://cran.r-project.org/src/contrib/htmltools_0.3.5.tar.gz
tar -xf htmltools_0.3.5.tar.gz
R CMD INSTALL htmltools

wget https://cran.r-project.org/src/contrib/httpuv_1.3.3.tar.gz
tar -xf httpuv_1.3.3.tar.gz
R CMD INSTALL httpuv

wget https://cran.r-project.org/src/contrib/xtable_1.8-2.tar.gz
tar -xf xtable_1.8-2.tar.gz
R CMD INSTALL xtable

wget https://cran.r-project.org/src/contrib/shiny_0.14.2.tar.gz
tar -xf shiny_0.14.2.tar.gz
R CMD INSTALL shiny

#wget https://cran.r-project.org/src/contrib/RcppEigen_0.3.2.8.1.tar.gz
#https://launchpad.net/ubuntu/+archive/primary/+files/r-cran-rcppeigen_0.3.2.8.1.orig.tar.gz
#tar -xf r-cran-rcppeigen_0.3.2.8.1.orig.tar.gz
#tar -xf RcppEigen_0.3.2.8.1.tar.gz
#R CMD INSTALL RcppEigen

wget https://cran.r-project.org/src/contrib/lme4_1.1-12.tar.gz
tar -xf lme4_1.1-12.tar.gz
R CMD INSTALL lme4

if [ ! -d $R_HOME/lib64/R/library/plyr ]; then
  echo -ne "installing plyr ..\r"
  wget http://cran.rstudio.com/src/contrib/plyr_1.8.4.tar.gz > $OPENSHIFT_LOG_DIR/installRplyr.log 2>&1
  bin/R CMD INSTALL plyr_1.8.4.tar.gz >> $OPENSHIFT_LOG_DIR/installRplyr.log 2>&1
  echo -ne "installing plyr ... done\r"
  echo ''
  rm plyr_1.8.4.tar.gz
else
  if [ -f plyr_1.8.4.tar.gz ]; then
    echo -ne "reinstalling plyr ..\r"
    bin/R CMD INSTALL plyr_1.8.4.tar.gz >> $OPENSHIFT_LOG_DIR/installRplyr.log 2>&1
    echo -ne "reinstalling plyr ...done\r"
    rm plyr_1.8.4.tar.gz
  else
    echo -ne"plyr already installed"
  fi
  echo ''
  echo -ne "plyr already installed\r"
  echo ''
fi

if [ ! -d $R_HOME/lib64/R/library/rCharts ]; then
  echo -ne "installing rCharts ..\r" 
  bin/Rscript -e "library('devtools');install_git('https://github.com/ramnathv/rCharts.git')" > $OPENSHIFT_LOG_DIR/installRcharts.log 2>&1
  echo -ne "installing rCharts ...done\r"
  echo ''
else
  echo -ne "rCharts already installed\r"
  echo ''
fi

