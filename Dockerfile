FROM rocker/r-ver:4.3.3
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev && \
    rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY . /app
RUN R -e 'install.packages("renv")'
RUN R -e 'renv::restore(prompt = FALSE)'
EXPOSE 3838 8000
CMD ["Rscript", "scripts/run_app.R"]
