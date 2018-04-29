FROM opencpu/base


RUN R -e 'devtools::install_github("imageqc/ovaryvisr")'


RUN \
  echo 'Redirect /index.html /ocpu/library/imageqc/www' > /etc/apache2/sites-enabled/app.conf
