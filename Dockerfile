FROM rocker/r-ver:4.3.3 AS build

RUN apt-get update && apt-get install -y     libcurl4-openssl-dev libssl-dev libxml2-dev curl     libgdal-dev libgeos-dev libproj-dev libicu-dev cmake &&     rm -rf /var/lib/apt/lists/*

WORKDIR /build
COPY . /build

RUN Rscript scripts/install_r_deps.R --all --test

RUN R CMD build . &&     R CMD check --no-manual --as-cran *.tar.gz

FROM rocker/r-ver:4.3.3

RUN apt-get update && apt-get install -y     libcurl4-openssl-dev libssl-dev libxml2-dev curl     libgdal-dev libgeos-dev libproj-dev libicu-dev &&     rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build /build /app

EXPOSE 3838 8000
HEALTHCHECK --interval=30s --timeout=5s CMD curl -f http://localhost:8000/health/live || exit 1

CMD ["Rscript", "scripts/run_app.R"]
