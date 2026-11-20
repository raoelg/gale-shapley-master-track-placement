FROM rocker/shiny:3.4.4
# Debian stretch is archived; point apt at archive.debian.org
RUN printf 'deb http://archive.debian.org/debian stretch main\n' > /etc/apt/sources.list \
 && rm -f /etc/apt/sources.list.d/* \
 && apt-get -o Acquire::Check-Valid-Until=false update -qq \
 && apt-get install -y --allow-unauthenticated -qq libcurl4-openssl-dev libssl-dev libxml2-dev \
 && rm -rf /var/lib/apt/lists/*
COPY packrat.lock /tmp/packrat.lock
COPY install_lock.R /tmp/install_lock.R
RUN Rscript /tmp/install_lock.R
COPY reinstall_ordered.R /tmp/reinstall_ordered.R
RUN Rscript /tmp/reinstall_ordered.R
RUN rm -rf /srv/shiny-server/*
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY app/ /srv/shiny-server/mastertrack/
# app directory owned by shiny (val.Rdata is a link to /dev/null, see below)
RUN chown -R shiny:shiny /srv/shiny-server/mastertrack
# the app saves val.Rdata (student data) on every run; send it to /dev/null so nothing is kept on disk
RUN ln -sf /dev/null /srv/shiny-server/mastertrack/val.Rdata
