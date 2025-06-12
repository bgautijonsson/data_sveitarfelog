library(tidyverse)
library(readxl)
library(janitor)
library(hagstofa)
library(googledrive)
library(googlesheets4)


#### Mannfjöldi ####

url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/2_byggdir/sveitarfelog/MAN02005.px"
mannfjoldi <- hg_data(url) |>
  filter(
    Aldur == "Alls",
    Kyn == "Alls",
    Sveitarfélag != "Alls"
  ) |>
  collect() |>
  clean_names() |>
  rename(mannfjoldi = 5) |>
  select(-aldur, -kyn) |>
  mutate(ar = parse_number(ar))

#### SÍS Gögn ####


rekstur <- read_excel("data-raw/Rekstrarreikningar 2023.xlsx", skip = 5) |> 
  clean_names() |> 
  fill(ar, sveitarfelag, hluti) |> 
  mutate(
    tegund2 = if_else(
      tegund2 == "Fjármagsliðir Total",
      "Fjármagnsliðir Total",
      tegund2
    ),
    tegund = if_else(
      tegund %in% c("Útsvar", "Fasteignaskattur", "Skattaígildi (lóðaleiga)"),
      "Skatttekjur án Jöfnunarsjóðs",
      tegund
    )
  ) |> 
  filter(
    tegund2 %in% c(
      "Gjöld Total", 
      "Tekjur Total",
      "Rekstrarniðurstaða Total",
      "Fjármagnsliðir Total",
      "Óreglulegir Total"
    ) | tegund %in% c(
      "Afskriftir", 
      "Framlag úr Jöfnunarsjóði", 
      "Útsvar",
      "Fasteignaskattur",
      "Laun og launatengd gjöld",
      "Skatttekjur án Jöfnunarsjóðs",
      "(Fjármagnsgjöld)"
    )
  ) |> 
  mutate(
    tegund2 = ifelse(is.na(tegund2), tegund, tegund2) |> 
      str_replace(" Total", "")
  ) |> 
  select(-tegund) |> 
  mutate(
    ar = parse_number(ar),
    sveitarfelag = str_sub(sveitarfelag, start = 6),
    total = coalesce(total, 0)
  ) |> 
  summarise(
    total = sum(total),
    .by = c(ar, sveitarfelag, hluti, tegund2)
  )




efnahagur <- read_excel("data-raw/Efnahagsreikningar 2023.xlsx", skip = 4) |> 
  clean_names() |> 
  fill(ar, sveitarfelag, hluti) |> 
  rename(
    tegund2 = flokkur_1
  ) |> 
  filter(
    tegund2 %in% c(
      "Veltufjármunir Total", 
      "Varanlegir rekstrarfjármunir", 
      "Áhættufjármunir og langtímakröfur Total",
      "Skuldbindingar", 
      "Langtímaskuldir",
      "Skammtímaskuldir", 
      "Eigið fé"
    ) | tegund %in% c(
      "Skammtímakröfur á eigin fyrirtæki",
      "Aðrir veltufjármunir",
      "Handbært fé",
      "Langtímakröfur á eigin fyrirtæki",
      "Langtímakröfur"
    )
  ) |> 
  mutate(
    tegund2 = ifelse(is.na(tegund2), tegund, tegund2) |>
      str_replace(" Total", "")
  ) |> 
  select(-tegund, -flokkur_2) |> 
  mutate(
    ar = parse_number(ar),
    sveitarfelag = str_sub(sveitarfelag, start = 6),
    total = coalesce(total, 0)
  )



sjodstreymi <- read_excel("data-raw/Sjodstreymi 2023.xlsx", skip = 4) |> 
  clean_names() |> 
  fill(ar, sveitarfelag, hluti) |> 
  rename(tegund2 = flokkur_2) |> 
  filter(
    tegund2 %in% c(
      "Veltufé frá rekstri Total", 
      "Fjárfestingarhreyfingar Total") | 
      tegund %in% c(
        "Afborganir langtímalána",
        "Aðrar fjármögnunarhreyfingar",
        "Tekin ný langtímalán",
        "Fjárfesting í varanlegum rekstrarfjármunum"
      )
  ) |> 
  mutate(
    tegund2 = ifelse(is.na(tegund), tegund2, tegund) |>
      str_replace(" Total", "")
  ) |> 
  select(-tegund) |> 
  mutate(
    ar = parse_number(ar),
    sveitarfelag = str_sub(sveitarfelag, start = 6),
    total = coalesce(total, 0)
  )


d <- efnahagur |> 
  pivot_wider(names_from = tegund2, values_from = total) |> 
  inner_join(
    rekstur |> 
      pivot_wider(names_from = tegund2, values_from = total),
    by = c("ar", "sveitarfelag", "hluti")
  ) |> 
  inner_join(
    sjodstreymi |> 
      pivot_wider(names_from = tegund2, values_from = total),
    by = c("ar", "sveitarfelag", "hluti")
  ) |> 
  mutate_at(
    vars(-ar, -sveitarfelag, -hluti),
    coalesce, 0
  )

#### Sameina Sveitarfélög ####

hunabyggd <- c(
  "Húnabyggð",
  "Skagabyggð"
)

vesturbyggd <- c(
  "Vesturbyggð",
  "Tálknafjarðarhreppur"
)


d <- d |>
  mutate(
    sveitarfelag = case_match(
      sveitarfelag,
      hunabyggd ~ "Húnabyggð",
      vesturbyggd ~ "Vesturbyggð",
      sveitarfelag ~ sveitarfelag
    )
  )

#### Reikna Breytur ####

d <- d |> mutate(
  heildarskuldir = `Skuldbindingar` + `Langtímaskuldir` + `Skammtímaskuldir`,
  eignir = `Varanlegir rekstrarfjármunir` + `Áhættufjármunir og langtímakröfur` + `Veltufjármunir`
)


d <- d |>
  select(
    ar,
    sveitarfelag,
    hluti,
    heildarskuldir,
    eignir,
    langtimaskuldir = "Langtímaskuldir",
    tekjur = "Tekjur",
    # utsvar = "Útsvar",
    # fasteignaskattur = "Fasteignaskattur",
    skatttekjur_an_jofnundarsjods = "Skatttekjur án Jöfnunarsjóðs",
    framlag_jofnunarsjods = "Framlag úr Jöfnunarsjóði",
    gjold = "Gjöld",
    afskriftir = "Afskriftir",
    fjarmagnslidir = "Fjármagnsliðir",
    fjarmagnsgjold = "(Fjármagnsgjöld)",
    oreglulegir_lidir = "Óreglulegir",
    rekstrarnidurstada = "Rekstrarniðurstaða",
    afborganir_langtimalana = "Afborganir langtímalána",
    tekin_ny_langtimalan = "Tekin ný langtímalán",
    adrar_fjarmognunarhreyfingar = "Aðrar fjármögnunarhreyfingar",
    ny_fjarfesting = "Fjárfesting í varanlegum rekstrarfjármunum",
    eigid_fe = "Eigið fé",
    veltufjarmunir = "Veltufjármunir",
    handbaert_fe = "Handbært fé",
    skammtimakrofur_eigin_fyrirtaeki = "Skammtímakröfur á eigin fyrirtæki",
    skammtimaskuldir = "Skammtímaskuldir",
    fjarfestingarhreyfingar = "Fjárfestingarhreyfingar",
    nyjar_langtimaskuldir = "Tekin ný langtímalán",
    launagjold = "Laun og launatengd gjöld",
    veltufe = "Veltufé frá rekstri",
    ahaettufjarmunir_og_langtimakrofur = "Áhættufjármunir og langtímakröfur",
    langtimakrofur_a_eigin_fyrirtaeki = "Langtímakröfur á eigin fyrirtæki",
    langtimakrofur = "Langtímakröfur"
  )

d <- d |>
  group_by(sveitarfelag, ar, hluti) |>
  summarise_at(
    vars(everything()),
    ~ 1000 * sum(.x)
  ) |>
  ungroup()

d <- d |>
  mutate(
    hluti = fct_recode(
      hluti,
      "A-hluti" = "A_hluti",
      "A og B-hluti" = "A_og_B_hluti"
    )
  )

d_2024 <- read_sheet("https://docs.google.com/spreadsheets/d/1SU7wVG_H38G5uzhdUc8mpOvwIW6aAj7PKYnmUB7IwgM/edit#gid=0") |>
  mutate_at(vars(-ar, -sveitarfelag, -hluti), \(x) x * 1000)


d <- d |>
  bind_rows(d_2024) |>
  inner_join(
    mannfjoldi,
    by = join_by(ar, sveitarfelag)
  )

d <- d |>
  mutate(
    eiginfjarhlutfall = eigid_fe / eignir,
    fjarf_nylan = tekin_ny_langtimalan - ny_fjarfesting,
    fjarfesting_hlutf_skuldir = fjarf_nylan / heildarskuldir,
    fjarmagnslidir_a_ibua = fjarmagnslidir / mannfjoldi,
    fjarmagnsgjold_a_ibua = fjarmagnsgjold / mannfjoldi,
    framlegd = tekjur - gjold + afskriftir,
    framlegd_hlutf = framlegd / tekjur,
    gjold_a_ibua = gjold / mannfjoldi,
    handbaert_fe_per_ibui = handbaert_fe / mannfjoldi,
    heildarskuldir = heildarskuldir,
    jofnunarsjodur_a_ibua = framlag_jofnunarsjods / mannfjoldi,
    launagjold_per_ibui = launagjold / mannfjoldi,
    launagjold_hlutf_gjold = launagjold / gjold,
    nettoskuldir = heildarskuldir - veltufjarmunir + skammtimakrofur_eigin_fyrirtaeki - langtimakrofur,
    nettoskuldir_hlutf_tekjur = nettoskuldir / tekjur,
    rekstrarnidurstada_hlutf = rekstrarnidurstada / tekjur,
    rekstrarnidurstada_a_ibua = rekstrarnidurstada / mannfjoldi,
    rekstur_3_ar = rekstrarnidurstada + lag(rekstrarnidurstada, 1) + lag(rekstrarnidurstada, 2),
    tekjur_3_ar = tekjur + lag(tekjur, 1) + lag(tekjur, 2),
    rekstur_3_ar_hlutf_tekjur = rekstur_3_ar / tekjur_3_ar,
    # utsvar_a_ibua = utsvar / mannfjoldi,
    # fasteignaskattur_a_ibua = fasteignaskattur / mannfjoldi,
    skattur_a_ibua = skatttekjur_an_jofnundarsjods / mannfjoldi,
    skuldahlutfall = 1 - eiginfjarhlutfall,
    skuldir_hlutf_tekjur = heildarskuldir / tekjur,
    skuldir_per_ibui = heildarskuldir / mannfjoldi,
    tekjur_a_ibua = tekjur / mannfjoldi,
    ny_langtimalan_a_ibua = tekin_ny_langtimalan / mannfjoldi,
    veltufe_hlutf_skuldir = veltufe / nettoskuldir,
    veltufe_hlutf_tekjur = veltufe / tekjur,
    veltufjarhlutfall = veltufjarmunir / skammtimaskuldir,
    veltufe_hlutf_afborganir = veltufe / afborganir_langtimalana,
    .by = c(sveitarfelag, hluti)
  )

d |> 
  write_csv(
    "data/total_data.csv"
  )

#### Þróunargögn ####

percent_vars <- c(
  "Eiginfjárhlutfall",
  "Framlegð sem hlutfall af tekjum",
  "Launa- og launatengd gjöld sem hlutfall af útgjöldum",
  "Nettóskuldir sem hlutfall af tekjum",
  "Rekstrarniðurstaða sem hlutfall af tekjum",
  "Rekstrarniðurstaða undanfarinna 3 ára sem hlutfall af tekjum",
  "Skuldir sem hlutfall af tekjum",
  "Veltufé frá rekstri sem hlutfall af tekjum",
  "Veltufjárhlutfall"
)

throun_data <- d |>
  select(
    sveitarfelag,
    ar,
    hluti,
    "Heildarskuldir" = heildarskuldir,
    "Eiginfjárhlutfall" = eiginfjarhlutfall,
    "Fjármagnsliðir á íbúa" = fjarmagnslidir_a_ibua,
    "Framlag jöfnunarsjóðs á íbúa" = jofnunarsjodur_a_ibua,
    "Framlegð sem hlutfall af tekjum" = framlegd_hlutf,
    "Handbært fé á íbúa" = handbaert_fe_per_ibui,
    "Launa- og launatengd gjöld á íbúa" = launagjold_per_ibui,
    "Launa- og launatengd gjöld sem hlutfall af útgjöldum" = launagjold_hlutf_gjold,
    "Nettóskuldir sem hlutfall af tekjum" = nettoskuldir_hlutf_tekjur,
    "Ný langtímalán á íbúa" = ny_langtimalan_a_ibua,
    "Rekstrarniðurstaða á íbúa" = rekstrarnidurstada_a_ibua,
    "Rekstrarniðurstaða sem hlutfall af tekjum" = rekstrarnidurstada_hlutf,
    "Rekstrarniðurstaða undanfarinna 3 ára sem hlutfall af tekjum" = rekstur_3_ar_hlutf_tekjur,
    # "Útsvar á íbúa" = utsvar_a_ibua,
    # "Fasteignaskattur á íbúa" = fasteignaskattur_a_ibua,
    "Útsvar og fasteignaskattur á íbúa" = skattur_a_ibua,
    "Skuldir sem hlutfall af tekjum" = skuldir_hlutf_tekjur,
    "Skuldir á íbúa" = skuldir_per_ibui,
    "Tekjur á íbúa" = tekjur_a_ibua,
    "Útgjöld á íbúa" = gjold_a_ibua,
    "Veltufé frá rekstri sem hlutfall af tekjum" = veltufe_hlutf_tekjur,
    "Veltufjárhlutfall" = veltufjarhlutfall
  ) |>
  pivot_longer(
    c(-sveitarfelag, -ar, -hluti),
    names_to = "name",
    values_to = "y"
  ) |>
  arrange(ar, hluti, sveitarfelag, name) |>
  mutate(
    is_percent = ifelse(
      name %in% percent_vars,
      TRUE,
      FALSE
    )
  )

throun_data |> 
  write_csv("data/throun_data.csv")

#### Dreifingargögn ####

percent_vars <- c(
  "Eiginfjárhlutfall",
  "Framlegð sem hlutfall af tekjum",
  "Handbært fé á íbúa",
  "Launa- og launatengd gjöld sem hlutfall af gjöldum",
  "Nettóskuldir sem hlutfall af tekjum",
  "Rekstrarniðurstaða sem hlutfall af tekjum",
  "Rekstrarniðurstaða undanfarinna 3 ára sem hlutfall af tekjum",
  "Skuldir sem hlutfall af tekjum",
  "Veltufé frá rekstri sem hlutfall af tekjum",
  "Veltufjárhlutfall"
)

dreifing_data <- d |>
  select(
    ar,
    sveitarfelag,
    hluti,
    "Eiginfjárhlutfall" = eiginfjarhlutfall,
    "Framlegð sem hlutfall af tekjum" = framlegd_hlutf,
    "Handbært fé á íbúa" = handbaert_fe_per_ibui,
    "Fjármagnsliðir á íbúa" = fjarmagnslidir_a_ibua,
    "Launa- og launatengd gjöld á íbúa" = launagjold_per_ibui,
    "Launa- og launatengd gjöld sem hlutfall af gjöldum" = launagjold_hlutf_gjold,
    "Nettóskuldir sem hlutfall af tekjum" = nettoskuldir_hlutf_tekjur,
    "Rekstrarniðurstaða sem hlutfall af tekjum" = rekstrarnidurstada_hlutf,
    "Rekstrarniðurstaða undanfarinna 3 ára sem hlutfall af tekjum" = rekstur_3_ar_hlutf_tekjur,
    # "Útsvar á íbúa" =  utsvar_a_ibua,
    # "Fasteignaskattur á íbúa" =  fasteignaskattur_a_ibua,
    "Útsvar og fasteignaskattur á íbúa" = skattur_a_ibua,
    "Skuldir sem hlutfall af tekjum" = skuldir_hlutf_tekjur,
    "Skuldir á íbúa" = skuldir_per_ibui,
    "Veltufé frá rekstri sem hlutfall af tekjum" = veltufe_hlutf_tekjur,
    "Veltufjárhlutfall" = veltufjarhlutfall
  ) |>
  pivot_longer(
    c(-sveitarfelag, -ar, -hluti),
    names_to = "name",
    values_to = "y"
  ) |>
  arrange(ar, hluti, sveitarfelag, name) |>
  mutate(
    is_percent = ifelse(
      name %in% percent_vars,
      TRUE,
      FALSE
    )
  )

dreifing_data |> 
  write_csv("data/dreifing_data.csv")

#### Viðmiðsgögn ####

vidmid_data <- d |>
  filter(
    ar >= 2010,
  ) |>
  select(
    sveitarfelag,
    ar,
    hluti,
    nettoskuldir_obs = nettoskuldir_hlutf_tekjur,
    rekstrarnidurstada_obs = rekstur_3_ar_hlutf_tekjur,
    framlegd_obs = framlegd_hlutf,
    veltufe_obs = veltufe_hlutf_tekjur,
    veltufjarhlutfall_obs = veltufjarhlutfall
  ) |>
  mutate(
    framlegd_vidmid = nettoskuldir_obs / 10,
    veltufe_vidmid = nettoskuldir_obs / 20,
    rekstrarnidurstada_vidmid = 0,
    veltufjarhlutfall_vidmid = 1,
    nettoskuldir_vidmid = 1
  ) |>
  pivot_longer(c(-sveitarfelag, -ar, -hluti), names_to = c("name", "type"), values_to = "value", names_sep = "_") |>
  pivot_wider(names_from = type, values_from = value) |>
  mutate(
    diff = obs - vidmid,
    colour = diff > 0
  )

vidmid_data |> 
  write_csv("data/vidmid_data.csv")
