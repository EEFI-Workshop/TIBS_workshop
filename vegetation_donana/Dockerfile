FROM eco4cast/rocker-neon4cast:4.2.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    libsecret-1-0 \
    libsecret-1-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    libgit2-dev \
    git \
    curl \
    python3-venv \
    python3-pip \
    python3-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install a more necessary R package into Docker - dependency of load_packages.R
RUN Rscript -e "install.packages('reticulate', repos='http://cran.rstudio.com/')"

WORKDIR /vegetation_donana

COPY ./load_packages.R ./load_packages.R

# Run load_packages.R - this command will stop after it is done
RUN Rscript load_packages.R

# Copy required files and folders to the container
COPY ./app.R ./submission_validator.R /vegetation_donana/
COPY ./R /vegetation_donana/R

#### CONTAINER = your virtual computer
CMD ["Rscript", "app.R"]

EXPOSE 3838
