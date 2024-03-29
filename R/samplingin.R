#' round_preserve_sum
#'
#' @param x a number
#' @param digits 0 (default)
round_preserve_sum = function(x, digits = 0) {
  up = 10 ^ digits
  x = x * up
  y = floor(x)
  indices = tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  return(y / up)
}

popCheck = function(sampl,pop){
  tot = 0
  for(i in 1:NROW(sampl)){
    if(sampl[i] >pop[i]){
      tot = tot +(sampl[i]-pop[i])
      sampl[i] <-pop[i]
    }
  }
  indx = order(pop,decreasing = T)
  j=1
  while(tot >0){
    if(j>NROW(pop)) j=1

    if(pop[indx[j]]-sampl[indx[j]]>0){
      sampl[indx[j]]=sampl[indx[j]]+1
      # show(tot)

      tot = tot-1
    }

    j=j+1
  }
  return(sampl)
}

#' Allocate Predetermined Allocations to Smaller Levels
#'
#' Allocate predetermined allocations to smaller levels using proportional allocation method
#'
#' @param data population tabulation dataframe
#' @param alokasi total allocation dataframe
#' @param group group of allocation level to be obtained
#' @param pop_var population variable in data
#' @param secondary how many times the secondary sample compares to primary sample
#'
#' @return allocation at more detailed level
#' @export
#' @examples
#'
#' library(samplingin)
#' library(magrittr)
#'
#' contoh_alokasi = alokasi_dt %>%
#'    dplyr::select(-n_primary) %>%
#'    dplyr::mutate(nasional = 1)
#'
#' alokasi_dt = get_allocation(
#'    data = contoh_alokasi
#'    , alokasi = 100
#'    , group = c("nasional")
#'    , pop_var = "jml_kabkota"
#' )
#'
get_allocation = function(data, alokasi, group, pop_var = "jml", secondary = 0){
  # if( is.na(falok) ){
  #   stop("Please select one variable as allocation sample")
  #   return()
  # }

  if( is.na(pop_var) ){
    stop("Please select one variable as population variable")
    return()
  }

  ret = data %>%
    mutate(
      fsqrt=sqrt(eval(parse(text = pop_var)))
    ) %>%
    group_by(.dots=group) %>%
    mutate(sfsqrt = sum(fsqrt)) %>%
    ungroup() %>%
    mutate(
      alokasi_n = alokasi
    ) %>%
    mutate(
      alok0 = fsqrt*alokasi_n/sfsqrt
    ) %>%
    group_by(.dots=group) %>%
    mutate(
      alok = round_preserve_sum(alok0)
      , alok_p = ceiling(secondary * alok)
    ) %>%
    mutate(
      alok_p = ifelse(alok+alok_p > eval(parse(text = pop_var)), eval(parse(text = pop_var)) - alok, alok_p)
    ) %>%
    ungroup()

  if(secondary == 0){
    ret = ret %>%
      select(names(data),alok) %>%
      rename(n_primary = alok)
  }else{
    ret = ret %>%
      select(names(data),alok,alok_p) %>%
      rename(
        n_primary = alok
        , n_secondary = alok_p
      )
  }


  # dd = ret %>%
  #   split(ret %>% select(group))
  #
  # ee = lapply(dd, function(x){
  #   x$alok = popCheck(x$alok, x$j_elig)
  #
  #   x
  # })
  #
  # ee = ee %>%
  #   bind_rows(.id = "id") %>%
  #   select(-id)
  #
  # ee

  return(ret)
}

# get_sample = function(data, n, ar, implicit = NA, method, auxVar=NA){
#   txt_info = ""
#   if( all(!is.na(implicit)) & all(implicit %in% names(data))){
#     data = data %>%
#       arrange_at(implicit)
#   }else{
#     txt_info = "implicit Variables not specified"
#     # warning("Tidak ada variabel implicit")
#   }
#
#   switch (
#     method,
#     "systematic" = {
#       obtain_sys = function(N,n){
#         # if(n>N){
#         #   stop("Number of sample is larger than number of population")
#         #   return()
#         # }
#
#         k = N/n
#         r = ar*k
#
#         ix_start = ifelse(r<1, 1, r)
#
#         ret = round(seq(ix_start, ix_start + k*(n-1), k), 0)
#       }
#
#       if(n > nrow(data)){
#         res = NULL
#         txt_info = "number of sample is larger than number of population"
#       }else{
#         res = res = data[obtain_sys(nrow(data), n), ]
#         txt_info = "success"
#       }
#
#       return(list(N = data %>% nrow, n = n, ar = ar, implicit=implicit, method = method, sampel = res, info = txt_info))
#     },
#     "pps" = {
#       if( is.na(auxVar) ){
#         stop("auxVar cannot be empty")
#         return()
#       }
#
#       return(list(N = data %>% nrow, n = nrow(res), ar = ar, implicit=implicit, method = method, sampel = NULL))
#     }
#   )
# }

createInterval = function(pop, method, strata, auxVar=NA, groupByVar=c("kdprov","kdkab")){
  if(all(c("VMIN","VMAX") %in% names(pop))){
    mRek = pop %>% select(-VMIN,-VMAX)
  }else{
    mRek = pop
  }
  switch (method,
          "systematic" = {
            mRek = mRek %>% group_by(.dots = groupByVar) %>%
              mutate(
                VMAX = ifelse(!is.na(!!rlang::parse_quo(strata, env = caller_env())), row_number(), 0),
                VMIN = ifelse(!is.na(!!rlang::parse_quo(strata, env = caller_env())), as.numeric(VMAX), 0)) %>%
              ungroup %>%
              mutate(INDEX = row_number())

            return(mRek)
          },
          "pps" = {
            if( is.na(auxVar) ){
              stop("auxVar cannot be empty")
              return()
            }

            # cat("Auxiliary variable is: ",auxVar,"\n")

            mRek = mRek %>% group_by(.dots = groupByVar) %>%
              mutate(VMAX = ifelse(!is.na(!!rlang::parse_quo(strata, env = caller_env())) & is.na(certainty),
                                   cumsum(eval(parse(text=auxVar)) * is.na(certainty)), 0),
                     VMIN = ifelse(!is.na(!!rlang::parse_quo(strata, env = caller_env())) & is.na(certainty),
                                   as.numeric(VMAX - eval(parse(text=auxVar)) + 1), 0) ) %>%
              ungroup %>%
              mutate(INDEX = row_number())

            return(mRek)
          }
  )
}

#' Select Samples Given its Parameters
#'
#' Samples selection using systematic or PPS (Probability Proportional to Size) sampling method.
#'
#' @param pop pop dataframe
#' @param alloc allocation dataframe
#' @param nsampel variable on alloc df as allocation sample
#' @param type type value for sample classification ('U' = Primary Samples, 'P' = Secondary Samples)
#' @param strata strata variable, must available on both pop and alloc dataframe
#' @param ident group by on allocation dataframe
#' @param implicitby variable used as implicit stratification
#' @param method method of sampling : `"systematic"` (the default) or `"pps"`
#' @param auxVar auxiliary variable for pps sampling (`method = "pps"`)
#' @param seed seed
#' @param predetermined_rn predetermined random number variable on allocation dataframe, the default value is NULL, random number will be generated randomly
#' @param verbose verbose (`TRUE` as default)
#'
#' @return list of population data (`"pop"`), selected samples (`"dsampel"`), and details of sampling process (`"rincian"`)
#' @export
#' @examples
#'
#' \donttest{
#' library(samplingin)
#' library(magrittr)
#' library(dplyr)
#'
#' # PPS Sampling
#' dtSampling_pps = doSampling(
#'    pop = pop_dt
#'    , alloc = alokasi_dt
#'    , nsampel = "n_primary"
#'    , type = "U"
#'    , ident = c("kdprov")
#'    , method = "pps"
#'    , auxVar = "Total"
#'    , seed = 1234
#' )
#'
#' # Population data with flag sample
#' pop_dt = dtSampling_pps$pop
#'
#' # Selected Samples
#' dsampel = dtSampling_pps$dsampel
#'
#' # Details of sampling process
#' rincian = dtSampling_pps$rincian
#'
#' # Systematic Sampling
#' dtSampling_sys = doSampling(
#'    pop = pop_dt
#'    , alloc = alokasi_dt
#'    , nsampel = "n_primary"
#'    , type = "U"
#'    , ident = c("kdprov")
#'    , method = "systematic"
#'    , seed = 4321
#' )
#'
#' # Population data with flag sample
#' pop_dt = dtSampling_sys$pop
#'
#' # Selected Samples
#' dsampel = dtSampling_sys$dsampel
#'
#' # Details of sampling process
#' rincian = dtSampling_sys$rincian
#'
#' # Systematic Sampling with predetermined random number (predetermined_rn parameter)
#'
#' alokasi_dt_rn = alokasi_dt %>% rowwise() %>% mutate(ar = runif(n(),0,1)) %>% ungroup
#'
#' dtSampling_sys = doSampling(
#'    pop = pop_dt
#'    , alloc = alokasi_dt_rn
#'    , nsampel = "n_primary"
#'    , type = "U"
#'    , ident = c("kdprov")
#'    , method = "systematic"
#'    , predetermined_rn = "ar"
#'    , seed = 4321
#' )
#'
#' # Population data with flag sample
#' pop_dt = dtSampling_sys$pop
#'
#' # Selected Samples
#' dsampel = dtSampling_sys$dsampel
#'
#' # Details of sampling process
#' rincian = dtSampling_sys$rincian
#' }
doSampling = function(pop, alloc, nsampel, type, strata=NULL, ident=c("kdprov","kdkab"), implicitby = NULL, method="systematic",
                      auxVar=NA, seed=1, predetermined_rn = NULL, verbose = TRUE){
  # Warning apabila syarat tidak terpenuhi
  if( length(method)==0 | length(method)>1 | !(method %in% c("systematic", "pps"))){
    stop("Please select one method. systematic or pps")
    return()
  }

  if( is.na(nsampel) ){
    stop("Please select one variable as allocation sample")
    return()
  }

  null_strata = 0

  if(is.null(strata)){
    null_strata = 1
    pop = pop %>%
      mutate(tmp_strata = as.integer(1))

    alloc = alloc %>%
      mutate(tmp_strata = as.integer(1))

    strata = "tmp_strata"
  }

  if( !is.null(implicitby) ){
    sortVar = c(ident, strata, implicitby)
    if(verbose){
      if(!null_strata){
        message("sort by: ",paste(ident, collapse = ", "),", ",paste(strata, collapse = ", ")," and ",paste(implicitby, collapse = ", "),"\n")
      }else{
        message("sort by: ",paste(ident, collapse = ", ")," and ",paste(implicitby, collapse = ", "),"\n")
      }
    }
  }else{
    sortVar = c(ident, strata)
    if(verbose){
      if(!null_strata){
        message("no implicit stratification variable chosen, sort by: ",paste(ident, collapse = ", ")," and ",paste(strata, collapse = ", "),"\n")
      }else{
        message("no implicit stratification variable chosen, sort by: ",paste(ident, collapse = ", "),"\n")
      }
    }
  }

  pop = pop %>%
    arrange_at(sortVar)

  # Variabel groupByVar merupakan gabungan parameter ident dan strata
  # sesuai dengan level group pada alokasi
  groupByVar = c(ident,strata)

  # Inisialisasi daftar sampel
  dsampel    = NULL

  # Inisialisasi variabel FLAGS (Flag Sampel) dan TANGGAL pada data populasi
  if(!("flags" %in% names(pop))){
    pop = pop %>% mutate(flags=NA)
  }

  if(!("tanggal" %in% names(pop))){
    pop = pop %>% mutate(tanggal=NA)
  }

  # flagS = match("FLAGS", colnames(pop))

  # filter out null allocation

  alokasi_minus = alloc %>%
    filter(eval(parse(text = nsampel)) < 0)

  alokasi_nol = alloc %>%
    filter(eval(parse(text = nsampel)) == 0)

  if(verbose){
    message("Negative allocation: ",nrow(alokasi_minus),"\n")

    if(nrow(alokasi_minus)>0){
      stop("Allocation cannot be negative")
    }

    message("Zero allocation: ",nrow(alokasi_nol),"\n")

    if(nrow(alokasi_nol)>0){
      message("Removing Zero allocation\n")
    }
  }

  alloc = alloc %>%
    filter(eval(parse(text = nsampel)) > 0)

  switch (method,
          "systematic" = {
            pop = createInterval(pop, method, strata, groupByVar = groupByVar)

            # Rekap populasi berdasarkan group
            mRek = pop %>%
              group_by(.dots = groupByVar) %>%
              summarise(npop = n()) %>%
              ungroup()

            # Membuat rincian penarikan sampel seperti
            # alokasi sampel per group,
            # populasi per group,
            # angka random yang dipakai, interval K, dan sisa penarikan sampel
            # seed digunakan untuk menetapkan angka random
            set.seed(seed)

            rincian = left_join(alloc, mRek, by=groupByVar) %>%
              mutate_at(c("npop"), ~replace(., is.na(.), 0)) %>%
              rowwise() %>%
              mutate(
                ar = ifelse(is.null(predetermined_rn), runif(n(),0,1), eval(parse(text = predetermined_rn))),
                npop=ifelse(!is.na(npop),npop,0),
                k = npop/eval(parse(text = nsampel)),
                sisa=9999) %>%
              ungroup() %>%
              as.data.frame()

            # Proses penarikan sampel per rincian alokasi
            for(i in 1:nrow(rincian)){
              # cat(i,"\n")
              if(rincian[i,nsampel] == 0) next

              # Apabila populasinya ternyata 0, maka variabel sisa = alokasi
              if(rincian[i,"npop"] == 0){
                rincian[i,"sisa"] = rincian[i,nsampel]
                next
              }

              # Inisialisasi list variabel
              lis         = list()

              # Membuat list variabel sesuai dengan parameter ident yang diberikan
              for(ii in 1:length(ident)){
                lis[[ident[ii]]] = rincian[i, ident[ii]]
              }

              # Membuat list variabel pelengkap lainnya
              lis[[strata]]  = rincian[i, strata]
              lis[[nsampel]]   = rincian[i, nsampel]
              lis$npop    = rincian[i,"npop"]
              lis$ar      = rincian[i,"ar"]
              lis$k       = rincian[i,"k"]

              if(verbose) cat("START: ")

              # Membuat kondisi penarikan sampel
              conds = ""
              for(ii in 1:length(groupByVar)){
                # Menuliskan pada console, progres penarikan sampelnya
                txt = eval(parse(text=paste0("lis[['",groupByVar[ii],"']]"))) %>% as.character()
                if( ii == length(groupByVar) ){
                  if(verbose) cat(" ",groupByVar[[ii]],txt,"\n")
                }else{
                  if(verbose) cat(" ",groupByVar[[ii]],txt)
                }

                # Penyusunan kondisi penarikan sampel sesuai
                # dengan rincian dan variabel group
                if( ii==1 ){
                  conds = ""
                }else{
                  conds = paste0(conds, " & ")
                }
                conds = paste0(conds, "eval(parse(text='",groupByVar[ii],"')) == lis[['",groupByVar[ii],"']] %>% as.character")
              }

              # Menyeleksi populasi sesuai dengan kondisi untuk
              # setiap rincian penarikan sampel
              y = pop %>%
                filter(eval(parse(text=conds))) %>%
                select(INDEX) %>% unlist %>% as.numeric

              # Inisialisasi variabel
              flag    = NULL
              dsts    = NULL
              st      = NULL
              nTemp=0
              ix = lis$ar * lis$k

              # Apabila jumlah alokasi lebih atau sama dengan populasi,
              # maka sampel diambil secara take all
              if(lis[[nsampel]] >= lis$npop){
                st = y
              }else{ # untuk alokasi < populasi
                while(nTemp<lis[[nsampel]]){
                  # fs=ifelse(ix < 1, ceiling(ix), round(ix))
                  if(ix < 1){
                    ix = ceiling(ix)
                    fs = ix
                  }else{
                    fs = round(ix)
                  }

                  flag  = c(flag, fs)
                  nTemp = nTemp + 1
                  ix    = ix + lis$k
                }
                st = y[flag]
              }

              # Menandai FLAGS dan TANGGAL sampel terpilih dan menyeleksinya dari populasi
              if(!is.null(st)){
                pop = pop %>%
                  mutate(
                    flags   = replace(flags, INDEX %in% st, type),
                    tanggal = replace(tanggal, INDEX %in% st, paste(format(Sys.Date(), format="%d/%m/%y"))))

                dsts = pop %>%
                  filter(INDEX %in% st)
              }

              # Menggabungkan sampel terpilih ke daftar sampel
              # Update variabel sisa pada rincian
              if (!is.null(dsts)) {
                # ix = match(c("VMIN","VMAX","INDEX"), colnames(dsts))
                # dsts[,ix] = NULL
                dsampel = rbindlist(list(dsampel, dsts), use.names = T, fill = T)
                rincian[i,"sisa"] = rincian[i,nsampel] - nrow(dsts)
              }
            }

            # Membuat rekap hasil penarikan sampel
            rek = dsampel %>%
              group_by(.dots = groupByVar) %>%
              summarise(jml=n()) %>%
              ungroup()

            # Join rincian dengan hasil penarikan sampel
            rincian = left_join(rincian, rek) %>%
              mutate_at(c("jml"), ~replace(., is.na(.), 0)) %>%
              as.data.frame()

            pop = pop %>%
              select(-VMIN,-VMAX,-INDEX) %>%
              as.data.frame()

            dsampel = dsampel %>%
              select(-VMIN,-VMAX,-INDEX) %>%
              as.data.frame()

            if(strata == "tmp_strata"){
              pop = pop %>%
                select(-tmp_strata)

              dsampel = dsampel %>%
                select(-tmp_strata)
            }

            sisa = rincian %>%
              summarise(sum(sisa, na.rm=T)) %>%
              pull()

            total = alloc %>%
              summarise(sum(eval(parse(text = nsampel)), na.rm=T))

            persentase = round((dsampel %>% nrow) * 100.0/total, 2)

            if(sisa>0){
              message("WARNING: There are still ",sisa," allocations for which samples have not been selected. Selected ", dsampel %>% nrow," out of ", total," (",persentase,"%)\n")
            }else{
              if(verbose) message("All allocations have been selected. Selected ", dsampel %>% nrow," out of ", total," (",persentase,"%)\n")
            }

            # Return value
            return(list(pop=pop , dsampel=dsampel, rincian=rincian))
          },
          "pps" = {
            # Berhenti apabila parameter auxVar kosong
            if( is.na(auxVar) ){
              stop("auxVar cannot be empty")
              return()
            }

            if(!("certainty" %in% names(pop))){
              pop = pop %>% mutate(certainty=NA)
            }

            isUlang = NA
            counter_certainty = 0
            while(is.na(isUlang) | isUlang==TRUE){
              counter_certainty = counter_certainty + 1
              cat("certainty counter: ", counter_certainty, "\n")

              # Membuat interval berdasarkan auxVar
              pop = createInterval(pop, method, strata, auxVar = auxVar, groupByVar = groupByVar)

              # Rekap populasi berdasarkan group
              mRek = pop %>%
                group_by(.dots = groupByVar) %>%
                summarise(
                  npop  = sum(eval(parse(text=auxVar))[is.na(certainty)], na.rm=T),
                  numobs = sum(is.na(certainty)),
                  ncertainty = sum(!is.na(certainty))
                ) %>%
                ungroup

              # Membuat rincian penarikan sampel seperti
              # alokasi sampel per group,
              # populasi per group,
              # angka random yang dipakai, interval K, dan sisa penarikan sampel
              # seed digunakan untuk menetapkan angka random
              set.seed(seed)
              rincian = left_join(alloc, mRek, by=groupByVar) %>%
                mutate_at(c("npop","numobs","ncertainty"), ~replace(., is.na(.), 0)) %>%
                rowwise() %>%
                mutate(
                  ar = ifelse(is.null(predetermined_rn), runif(n(),0,1), eval(parse(text = predetermined_rn))),
                  k  = ifelse(npop>0, npop/eval(parse(text = nsampel)), NA),
                  sisa=9999,
                  nsam = eval(parse(text = nsampel)) - ncertainty
                ) %>%
                ungroup() %>%
                as.data.frame()

              # certainty selected (filter auxVar yang lebih dari atau sama dengan interval)
              certainty_pop = pop %>%
                left_join(
                  rincian %>%
                    select(groupByVar,k)
                ) %>%
                filter(!is.na(k) & k <= eval(parse(text = auxVar)) & is.na(certainty))

              if(nrow(certainty_pop) > 0){
                certainty_s = pop %>%
                  left_join(
                    rincian %>%
                      select(groupByVar,nsam,k)
                  ) %>%
                  filter(!is.na(k) & k <= eval(parse(text = auxVar)) & is.na(certainty)) %>%
                  group_by(.dots = groupByVar) %>%
                  mutate(cert_now = n()) %>%
                  sample_n(min(nsam, cert_now)) %>%
                  ungroup() %>%
                  pull(INDEX)
              }else{
                certainty_s = NULL
              }

              if(length(certainty_s)>0){
                isUlang = TRUE

                pop = pop %>%
                  mutate(
                    flags   = replace(flags, INDEX %in% certainty_s, type),
                    tanggal = replace(tanggal, INDEX %in% certainty_s, paste(format(Sys.Date(), format="%d/%m/%y"))),
                    certainty = replace(certainty, INDEX %in% certainty_s, "1"),
                  )
              }else{
                isUlang = FALSE

                rincian = rincian %>%
                  mutate(sisa = nsam)

                dsampel = pop %>%
                  filter(!is.na(flags))
              }
            }

            # Proses penarikan sampel per rincian alokasi
            for(i in 1:nrow(rincian)){
              if(rincian[i,"nsam"] == 0) next

              # Apabila populasinya ternyata 0, maka variabel sisa = alokasi
              if(rincian[i,"numobs"] == 0){
                rincian[i,"sisa"] = rincian[i, "nsam"]
                next
              }

              # Proses penarikan sampel pps, apabila ada 1 BS yang terpilih
              # lebih dari satu kali, penarikan sampel diulang
              # menggunakan angka random baru
              isUlang = NA
              while(is.na(isUlang) | isUlang==TRUE){
                # Update AR apabila 1 BS terpilih lebih dari 1 kali
                if(isUlang==TRUE & !is.na(isUlang)){
                  rincian[i,"ar"] = runif(1,0,1)
                }
                isUlang = FALSE

                # Inisialisasi list variabel
                lis         = list()

                # Membuat list variabel sesuai dengan parameter ident yang diberikan
                for(ii in 1:length(ident)){
                  lis[[ident[ii]]] = rincian[i, ident[ii]]
                }

                # Membuat list variabel pelengkap lainnya
                lis[[strata]]  = rincian[i,strata]
                lis[[nsampel]] = rincian[i,nsampel]
                lis$nsam = rincian[i,"nsam"]
                lis$npop    = rincian[i,"npop"]
                lis$numobs  = rincian[i,"numobs"]
                lis$ar      = rincian[i,"ar"]
                lis$k       = rincian[i,"k"]

                # ==cat("START:")

                # Membuat kondisi penarikan sampel
                conds = ""
                for(ii in 1:length(groupByVar)){
                  # Menuliskan pada console, progres penarikan sampelnya
                  txt = eval(parse(text=paste0("lis[['",groupByVar[ii],"']]"))) %>% as.character()
                  if( ii == length(groupByVar) ){
                    if(verbose) cat(" ",groupByVar[[ii]],txt,"\n")
                  }else{
                    if(verbose) cat(" ",groupByVar[[ii]],txt)
                  }

                  # Penyusunan kondisi penarikan sampel sesuai
                  # dengan rincian dan variabel group
                  if( ii==1 ){
                    conds = ""
                  }else{
                    conds = paste0(conds, " & ")
                  }
                  conds = paste0(conds, "eval(parse(text='",groupByVar[ii],"')) == lis[['",groupByVar[ii],"']] %>% as.character")
                }

                # Menyeleksi populasi sesuai dengan kondisi untuk
                # setiap rincian penarikan sampel
                y = pop %>%
                  filter(eval(parse(text=conds)) & is.na(certainty)) %>%
                  select(VMIN,VMAX,INDEX)

                # y = pop %>%
                #   filter(PROV==lis[["PROV"]] & KAB==lis[["KAB"]] & STRATA==lis[["STRATA"]]) %>%
                #   select(INDEX) %>% unlist %>% as.numeric

                # Inisialisasi variabel
                flag    = NULL
                dsts    = NULL
                st      = NULL
                nTemp=0
                last_fs = 0
                ix = lis$ar * lis$k

                # Apabila jumlah alokasi lebih dari populasi,
                # maka revisi alokasi
                if(lis[["nsam"]] > nrow(y)){
                  txt_rincian = ""
                  for(ggi in 1:length(groupByVar)){
                    txt_rincian = paste(txt_rincian, lis[[ groupByVar[ggi] ]])
                  }

                  if(verbose) cat("Excess Allocation, check ==> (",txt_rincian,") || Allocation: ", lis[["nsam"]]," :: Selected Sample: ",nrow(y), "\n")
                  # return()

                  st = y
                }else{
                  while(nTemp<as.numeric(lis[["nsam"]]) & !isUlang){
                    fs = y %>%
                      filter(as.numeric(ceiling(ix))>=VMIN & as.numeric(ceiling(ix))<=VMAX) %>%
                      select(INDEX) %>%
                      unlist %>% as.numeric

                    # cat(nTemp," == ",as.numeric(ix), " == ", fs,"\n")
                    # Cek apakah BS terpilih lebih dari sekali
                    if(fs == last_fs){
                      # if(verbose) cat(lis[["ar"]],"\n")
                      if(verbose) cat("ix: ",as.numeric(ix)," last_fs: ",last_fs," fs: ",fs,"\n")
                      if(verbose) cat("SAME INDEX (REPEAT PPS Sampling)\n")
                      isUlang = TRUE
                    } else {
                      last_fs = fs
                    }

                    flag  = c(flag, fs)
                    nTemp = nTemp + 1

                    ix    = ix + lis$k
                  }
                  st = flag
                }

                # Menandai FLAGS dan TANGGAL sampel terpilih dan menyeleksinya dari populasi
                if(!is.null(st) & !isUlang){
                  pop = pop %>%
                    mutate(flags   = replace(flags, INDEX %in% st, type),
                           tanggal = replace(tanggal, INDEX %in% st, paste(format(Sys.Date(), format="%d/%m/%y"))))

                  dsts = pop %>%
                    filter(INDEX %in% st)
                }

                # Menggabungkan sampel terpilih ke daftar sampel
                # Update variabel sisa pada rincian
                if (!is.null(dsts) & !isUlang) {
                  # ix = match(c("VMIN","VMAX","INDEX"), colnames(dsts))
                  # dsts[,ix] = NULL
                  dsampel = rbindlist(list(dsampel, dsts), use.names = T, fill = T)
                  rincian[i,"sisa"] = rincian[i,nsampel] - nrow(dsts)
                }
              }
            }

            # Membuat rekap hasil penarikan sampel
            rek = dsampel %>%
              group_by(.dots = groupByVar) %>%
              summarise(jml=n()) %>%
              ungroup()

            # Join rincian dengan hasil penarikan sampel dan update variabel sisa
            rincian = left_join(rincian, rek) %>%
              mutate_at(c("jml"), ~replace(., is.na(.), 0)) %>%
              mutate(sisa = eval(parse(text = nsampel)) - jml) %>%
              as.data.frame()

            pop = pop %>%
              select(-VMIN,-VMAX,-INDEX) %>%
              as.data.frame()

            dsampel = dsampel %>%
              select(-VMIN,-VMAX,-INDEX) %>%
              as.data.frame()

            if(strata == "tmp_strata"){
              pop = pop %>%
                select(-tmp_strata)

              dsampel = dsampel %>%
                select(-tmp_strata)
            }

            sisa = rincian %>%
              summarise(sum(sisa, na.rm=T)) %>%
              pull()

            total = alloc %>%
              summarise(sum(eval(parse(text = nsampel)), na.rm=T))

            persentase = round((dsampel %>% nrow) * 100.0/total, 2)

            if(sisa>0){
              message("WARNING: There are still ",sisa," allocations for which samples have not been selected. Selected ", dsampel %>% nrow," out of ", total," (",persentase,"%)\n")
            }else{
              if(verbose) message("All allocations have been selected. Selected ", dsampel %>% nrow," out of ", total," (",persentase,"%)\n")
            }

            # Return value
            return(list(pop=pop, dsampel=dsampel, rincian=rincian))
          }
  )

}
