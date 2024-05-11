## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(samplingin)
library(dplyr)
library(magrittr)
set.seed(114)

## -----------------------------------------------------------------------------
dim(pop_dt)

pop_dt %>% head()

## -----------------------------------------------------------------------------
dim(alokasi_dt)

alokasi_dt

## -----------------------------------------------------------------------------
dtSampling_srs = doSampling(
  pop     = pop_dt,
  alloc   = alokasi_dt,
  nsample = "n_primary",
  seed    = 7891,
  method  = "srs",
  ident   = c("kdprov"),
  type    = "U"
)

## -----------------------------------------------------------------------------
head(dtSampling_srs$pop)

## -----------------------------------------------------------------------------
head(dtSampling_srs$sampledf)

dtSampling_srs$sampledf %>% nrow

## -----------------------------------------------------------------------------
head(dtSampling_srs$details)

## -----------------------------------------------------------------------------
dtSampling_u = doSampling(
  pop     = pop_dt,
  alloc   = alokasi_dt,
  nsample = "n_primary",
  seed    = 2,
  method  = "systematic",
  ident   = c("kdprov"),
  type    = "U"
)

## -----------------------------------------------------------------------------
head(dtSampling_u$pop)

## -----------------------------------------------------------------------------
head(dtSampling_u$sampledf)

dtSampling_u$sampledf %>% nrow

## -----------------------------------------------------------------------------
head(dtSampling_u$details)

## -----------------------------------------------------------------------------
alokasi_dt_p = alokasi_dt %>% 
  mutate(n_secondary = 2*n_primary)

dtSampling_p = doSampling(
  pop     = dtSampling_u$pop,
  alloc   = alokasi_dt_p,
  nsample = "n_secondary",
  seed    = 243,
  method  = "systematic",
  ident   = c("kdprov"),
  type    = "P",
  is_secondary = TRUE
)

## -----------------------------------------------------------------------------
dtSampling_p$details %>% 
  filter(n_deficit>0)

## -----------------------------------------------------------------------------
head(dtSampling_p$pop)

## -----------------------------------------------------------------------------
dtSampling_p$pop %>% count(flags)

## -----------------------------------------------------------------------------
head(dtSampling_p$sampledf)

dtSampling_p$sampledf %>% nrow

## -----------------------------------------------------------------------------
head(dtSampling_p$details)

## -----------------------------------------------------------------------------
dtSampling_pps = doSampling(
  pop     = pop_dt,
  alloc   = alokasi_dt,
  nsample = "n_primary",
  seed    = 321,
  method  = "pps",
  auxVar  = "Total",
  ident   = c("kdprov"),
  type    = "U"
)

## -----------------------------------------------------------------------------
head(dtSampling_pps$pop)

## -----------------------------------------------------------------------------
head(dtSampling_pps$sampledf)

dtSampling_pps$sampledf %>% nrow

## -----------------------------------------------------------------------------
head(dtSampling_pps$details)

## -----------------------------------------------------------------------------
pop_dt_strata = pop_dt %>% 
  mutate(
    strata_kabkot = ifelse(substr(kdkab,1,1)=='7', 2, 1)
  )

alokasi_dt_strata = pop_dt_strata %>% 
  group_by(kdprov,strata_kabkot) %>% 
  summarise(
    jml_kabkota = n()
  ) %>% 
  ungroup %>% 
  left_join(
    alokasi_dt %>% 
      select(kdprov,n_primary) %>% 
      rename(n_alloc = n_primary)
  )
  
alokasi_dt_strata = alokasi_dt_strata %>%
  get_allocation(n_alloc = "n_alloc", group = c("kdprov"), pop_var = "jml_kabkota")

dtSampling_strata = doSampling(
  pop     = pop_dt_strata,
  alloc   = alokasi_dt_strata,
  nsample = "n_primary",
  seed    = 3512,
  method  = "systematic",
  strata  = "strata_kabkot",
  ident   = c("kdprov"),
  type    = "U"
)

## -----------------------------------------------------------------------------
head(dtSampling_strata$pop)

## -----------------------------------------------------------------------------
head(dtSampling_strata$sampledf)

dtSampling_strata$sampledf %>% nrow

dtSampling_strata$sampledf %>% count(strata_kabkot)


## -----------------------------------------------------------------------------
head(dtSampling_strata$details)

## -----------------------------------------------------------------------------
dtSampling_implicit = doSampling(
  pop        = pop_dt_strata,
  alloc      = alokasi_dt_strata,
  nsample    = "n_primary",
  seed       = 3512,
  method     = "systematic",
  strata     = "strata_kabkot",
  implicitby = "Total",
  ident      = c("kdprov"),
  type       = "U"
)

## -----------------------------------------------------------------------------
head(dtSampling_implicit$pop)

## -----------------------------------------------------------------------------
head(dtSampling_implicit$sampledf)

dtSampling_implicit$sampledf %>% nrow

dtSampling_implicit$sampledf %>% count(strata_kabkot)


## -----------------------------------------------------------------------------
head(dtSampling_implicit$details)

## -----------------------------------------------------------------------------
set.seed(988)
alokasi_dt_arand = alokasi_dt_strata %>%
  mutate(arand = runif(n(),0,1))

alokasi_dt_arand %>% as.data.frame %>% head(10)

dtSampling_prn = doSampling(
  pop        = pop_dt_strata,
  alloc      = alokasi_dt_arand,
  nsample    = "n_primary",
  seed       = 974,
  method     = "systematic",
  strata     = "strata_kabkot",
  predetermined_rn = "arand",
  ident      = c("kdprov"),
  type       = "U"
)

## -----------------------------------------------------------------------------
head(dtSampling_prn$pop)

## -----------------------------------------------------------------------------
head(dtSampling_prn$sampledf)

dtSampling_prn$sampledf %>% nrow


## -----------------------------------------------------------------------------
head(dtSampling_prn$details)

## -----------------------------------------------------------------------------
set.seed(242)
alokasi_prov = alokasi_dt %>%
  select(-jml_kabkota, -n_primary) %>%
  mutate(init_alloc = as.integer(runif(n(), 100, 200))) %>%
  as.data.frame()

alokasi_prov %>% head(10)

alokasi_prov %>% summarise(sum(init_alloc))

alokasi_kab = pop_dt %>%
  left_join(alokasi_prov) %>%
  get_allocation(n_alloc = "init_alloc", group = c("kdprov"), pop_var = "Total") %>%
  as.data.frame()

alokasi_kab %>% head(10)

alokasi_kab %>% summarise(sum(n_primary))

