# Based on Centos 6
FROM centos:6

# Install dependencies
RUN yum localinstall -y http://yum.postgresql.org/9.4/redhat/rhel-6-x86_64/pgdg-centos94-9.4-1.noarch.rpm
RUN yum install -y postgresql94-devel postgresql94-contrib

# Add assets
ADD live-profile-server-backend/static /opt/server/static

# Add executable
ADD live-profile-server-backend/live-profile-server /opt/server/live-profile-server

# Default folder
WORKDIR /opt/server