
# ============================================================
# MEMULIHKAN DATA UNTUK TAHAP 2
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)
library(writexl)
library(stringr)

# Pilih kembali folder dataset hasil ekstrak

folder_data <- choose.dir(
  caption = "Pilih folder Dataset Gelar Rasa 2026"
)

if (is.na(folder_data)) {
  stop("Folder dataset belum dipilih!")
}

# Folder output

folder_output <- file.path(
  folder_data,
  "HASIL_DATA_AUDIT"
)

# Membaca kembali 27 file AQMS

file_aqms <- list.files(
  path = folder_data,
  pattern = "_AQMS\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

data_aqms_list <- map(
  file_aqms,
  ~ read_csv(
    .x,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
)

names(data_aqms_list) <- tools::file_path_sans_ext(
  basename(file_aqms)
)

# Menggabungkan AQMS

aqms_raw <- imap_dfr(
  data_aqms_list,
  ~ mutate(.x, station_file = .y)
)

# Membaca Health

file_health <- list.files(
  path = folder_data,
  pattern = "health_outcomes_daily\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

health_raw <- read_csv(
  file_health[1],
  show_col_types = FALSE
)

# Memeriksa hasil pemulihan

dim(aqms_raw)
dim(health_raw)

length(data_aqms_list)

exists("aqms_raw")
exists("health_raw")
exists("folder_output")



# ============================================================
# GELAR RASA 2026
# TAHAP 2 - DATA CLEANING & PREPROCESSING
# ============================================================


# ============================================================
# BAGIAN 0. MEMANGGIL PACKAGE DAN MEMERIKSA OBJEK
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)
library(writexl)
library(stringr)

# Memastikan objek yang diperlukan tersedia

stopifnot(
  exists("aqms_raw"),
  exists("health_raw"),
  exists("data_aqms_list"),
  exists("folder_output")
)

# Memastikan folder output tersedia

if (!dir.exists(folder_output)) {
  dir.create(folder_output, recursive = TRUE)
}


# ============================================================
# BAGIAN 1. MENENTUKAN KOLOM NUMERIK AQMS
# ============================================================

kolom_angka <- c(
  "pm25_ug_m3",
  "pm10_ug_m3",
  "so2_ug_m3",
  "co_ug_m3",
  "o3_ug_m3",
  "no2_ug_m3",
  "pm25_ppm",
  "pm10_ppm",
  "so2_ppm",
  "co_ppm",
  "o3_ppm",
  "no2_ppm",
  "temperature_c",
  "humidity_pct",
  "wind_speed_ms",
  "wind_direction_deg"
)

# Hanya menggunakan kolom yang tersedia

kolom_angka <- intersect(
  kolom_angka,
  names(aqms_raw)
)


# ============================================================
# BAGIAN 2. KONVERSI TIPE DATA AQMS
# ============================================================

# Fungsi konversi angka
# Nilai asli tetap tersimpan di aqms_raw

ubah_ke_angka <- function(x) {
  as.numeric(trimws(as.character(x)))
}

aqms_clean <- aqms_raw %>%
  
  mutate(
    
    across(
      all_of(kolom_angka),
      ubah_ke_angka
    ),
    
    timestamp_start = ymd_hms(
      as.character(timestamp_start),
      quiet = TRUE
    ),
    
    timestamp_end = ymd_hms(
      as.character(timestamp_end),
      quiet = TRUE
    ),
    
    qc_status = str_to_upper(
      str_trim(qc_flag)
    )
  )

glimpse(aqms_clean)


# ============================================================
# BAGIAN 3. MEMBUAT PENANDA TIMESTAMP
# ============================================================

aqms_clean <- aqms_clean %>%
  
  mutate(
    
    # Timestamp tahun 1970
    
    flag_tahun_1970 = coalesce(
      year(timestamp_start) == 1970,
      FALSE
    ),
    
    # Timestamp kosong atau gagal dibaca
    
    flag_timestamp_missing =
      is.na(timestamp_start) |
      is.na(timestamp_end),
    
    # Waktu mulai lebih besar daripada waktu selesai
    
    flag_interval_invalid =
      !is.na(timestamp_start) &
      !is.na(timestamp_end) &
      timestamp_start > timestamp_end
  )

# Melihat jumlah masalah timestamp

ringkasan_timestamp <- aqms_clean %>%
  
  summarise(
    timestamp_1970 = sum(flag_tahun_1970),
    timestamp_missing = sum(flag_timestamp_missing),
    interval_invalid = sum(flag_interval_invalid)
  )

print(ringkasan_timestamp)


# ============================================================
# BAGIAN 4. MENANDAI DATA DUPLIKAT
# ============================================================

# Duplikat berdasarkan kombinasi stasiun, sensor,
# dan timestamp_start.
# Semua anggota kelompok duplikat akan ditandai.

aqms_clean <- aqms_clean %>%
  
  add_count(
    station_file,
    sensor_id,
    timestamp_start,
    name = "jumlah_kombinasi_waktu"
  ) %>%
  
  mutate(
    
    flag_duplikat_waktu =
      !is.na(sensor_id) &
      !is.na(timestamp_start) &
      jumlah_kombinasi_waktu > 1
  )

# Menghitung jumlah baris yang ditandai duplikat

ringkasan_duplikat <- aqms_clean %>%
  
  summarise(
    jumlah_baris_duplikat =
      sum(flag_duplikat_waktu),
    
    jumlah_kelompok_duplikat =
      n_distinct(
        paste(
          station_file,
          sensor_id,
          timestamp_start
        )[flag_duplikat_waktu]
      )
  )

print(ringkasan_duplikat)


# ============================================================
# BAGIAN 5. MENANDAI STATUS QC
# ============================================================

aqms_clean <- aqms_clean %>%
  
  mutate(
    
    flag_qc = case_when(
      
      qc_status == "VALID" ~ "VALID",
      
      is.na(qc_status) | qc_status == "" ~ "MISSING",
      
      TRUE ~ "NON_VALID"
    )
  )

# Ringkasan QC

ringkasan_qc <- aqms_clean %>%
  
  count(
    station_file,
    flag_qc,
    name = "jumlah_data"
  ) %>%
  
  group_by(station_file) %>%
  
  mutate(
    persentase =
      jumlah_data / sum(jumlah_data) * 100
  ) %>%
  
  ungroup()

print(ringkasan_qc)


# ============================================================
# BAGIAN 6. MEMERIKSA NILAI TIDAK WAJAR
# ============================================================

# Pemeriksaan dasar:
# - Konsentrasi polutan negatif
# - Kelembapan di luar 0-100%
# - Kecepatan angin negatif
# - Arah angin di luar 0-360 derajat

kolom_polutan <- c(
  "pm25_ug_m3",
  "pm10_ug_m3",
  "so2_ug_m3",
  "co_ug_m3",
  "o3_ug_m3",
  "no2_ug_m3",
  "pm25_ppm",
  "pm10_ppm",
  "so2_ppm",
  "co_ppm",
  "o3_ppm",
  "no2_ppm"
)

kolom_polutan <- intersect(
  kolom_polutan,
  names(aqms_clean)
)

aqms_clean <- aqms_clean %>%
  
  mutate(
    
    flag_polutan_negatif = if_any(
      all_of(kolom_polutan),
      ~ !is.na(.x) & .x < 0
    ),
    
    flag_humidity_invalid =
      !is.na(humidity_pct) &
      (
        humidity_pct < 0 |
          humidity_pct > 100
      ),
    
    flag_wind_speed_invalid =
      !is.na(wind_speed_ms) &
      wind_speed_ms < 0,
    
    flag_wind_direction_invalid =
      !is.na(wind_direction_deg) &
      (
        wind_direction_deg < 0 |
          wind_direction_deg > 360
      )
  )

# Ringkasan nilai tidak wajar

ringkasan_nilai <- aqms_clean %>%
  
  summarise(
    
    polutan_negatif =
      sum(flag_polutan_negatif),
    
    humidity_invalid =
      sum(flag_humidity_invalid),
    
    wind_speed_invalid =
      sum(flag_wind_speed_invalid),
    
    wind_direction_invalid =
      sum(flag_wind_direction_invalid)
  )

print(ringkasan_nilai)


# ============================================================
# BAGIAN 7. MEMBUAT DATA AQMS UNTUK ANALISIS
# ============================================================

# Dataset ini hanya mengambil data QC VALID
# dan timestamp yang lolos pemeriksaan.
#
# Duplikat dan nilai tidak wajar tetap ditandai,
# belum otomatis dihapus.

aqms_analysis <- aqms_clean %>%
  
  filter(
    
    flag_qc == "VALID",
    
    !flag_tahun_1970,
    
    !flag_timestamp_missing,
    
    !flag_interval_invalid
  ) %>%
  
  mutate(
    
    tanggal = as.Date(timestamp_start),
    
    jam = hour(timestamp_start),
    
    hari = wday(
      timestamp_start,
      label = TRUE,
      abbr = FALSE
    )
  )

# Dataset sensitivitas:
# Mengeluarkan baris duplikat dari dataset analisis.
# Gunakan untuk membandingkan hasil, bukan menggantikan
# dataset utama secara otomatis.

aqms_analysis_no_duplicate <- aqms_analysis %>%
  
  filter(!flag_duplikat_waktu)

# Memeriksa ukuran kedua dataset

dim(aqms_analysis)

dim(aqms_analysis_no_duplicate)


# ============================================================
# BAGIAN 8. MEMBERSIHKAN DATA HEALTH
# ============================================================

kolom_health_angka <- c(
  "temp_c",
  "dptp_c",
  "rhum_pct",
  "resp_admissions_under5",
  "resp_admissions_over60",
  "asthma_emergency_cases",
  "copd_admissions",
  "resp",
  "cvd",
  "death"
)

kolom_health_angka <- intersect(
  kolom_health_angka,
  names(health_raw)
)

health_clean <- health_raw %>%
  
  mutate(
    
    date = as.Date(
      as.character(date)
    ),
    
    across(
      all_of(kolom_health_angka),
      ubah_ke_angka
    )
  )

# Penanda jumlah kasus negatif

kolom_kasus <- c(
  "resp_admissions_under5",
  "resp_admissions_over60",
  "asthma_emergency_cases",
  "copd_admissions",
  "resp",
  "cvd",
  "death"
)

kolom_kasus <- intersect(
  kolom_kasus,
  names(health_clean)
)

health_clean <- health_clean %>%
  
  mutate(
    
    flag_kasus_negatif = if_any(
      all_of(kolom_kasus),
      ~ !is.na(.x) & .x < 0
    ),
    
    flag_tanggal_missing = is.na(date)
  )

# Dokumentasi definisi variabel sesuai pengumuman panitia

kamus_health_revisi <- tibble(
  
  nama_variabel = c(
    "resp_admissions_over60",
    "resp_admissions_under5",
    "asthma_emergency_cases",
    "copd_admissions",
    "resp"
  ),
  
  definisi = c(
    "Jumlah rawat inap akibat gangguan pernapasan (ISPA) pada lansia di atas 60 tahun, sesuai klarifikasi panitia.",
    "Rawat inap gangguan pernapasan pada anak di bawah 5 tahun; definisi spesifik perlu merujuk dokumentasi dataset.",
    "Jumlah kasus kegawatdaruratan asma.",
    "Jumlah rawat inap akibat COPD.",
    "Jumlah kasus gangguan pernapasan umum; tidak otomatis disamakan dengan ISPA."
  )
)

print(kamus_health_revisi)


# ============================================================
# BAGIAN 9. MENANDAI DUPLIKAT DATA HEALTH
# ============================================================

# Pemeriksaan kombinasi tanggal dan rumah sakit

health_clean <- health_clean %>%
  
  add_count(
    hospital_id,
    date,
    name = "jumlah_hospital_tanggal"
  ) %>%
  
  mutate(
    
    flag_duplikat_hospital_tanggal =
      !is.na(hospital_id) &
      !is.na(date) &
      jumlah_hospital_tanggal > 1
  )

ringkasan_health <- tibble(
  
  pemeriksaan = c(
    "Jumlah baris Health awal",
    "Tanggal missing",
    "Kasus negatif",
    "Baris dengan kombinasi rumah sakit dan tanggal berulang"
  ),
  
  jumlah = c(
    
    nrow(health_raw),
    
    sum(health_clean$flag_tanggal_missing),
    
    sum(health_clean$flag_kasus_negatif),
    
    sum(health_clean$flag_duplikat_hospital_tanggal)
  )
)

print(ringkasan_health)


# ============================================================
# BAGIAN 10. RINGKASAN HASIL CLEANING AQMS
# ============================================================

ringkasan_cleaning <- tibble(
  
  pemeriksaan = c(
    "Total data AQMS awal",
    "Total data AQMS setelah konversi",
    "Data QC VALID",
    "Data NON-VALID",
    "Data QC MISSING",
    "Timestamp tahun 1970",
    "Timestamp missing",
    "Interval waktu tidak valid",
    "Baris bertanda duplikat",
    "Baris polutan negatif",
    "Baris humidity tidak valid",
    "Baris wind speed tidak valid",
    "Baris wind direction tidak valid",
    "Data AQMS untuk analisis utama",
    "Data AQMS tanpa duplikat untuk sensitivitas"
  ),
  
  jumlah = c(
    
    nrow(aqms_raw),
    
    nrow(aqms_clean),
    
    sum(aqms_clean$flag_qc == "VALID"),
    
    sum(aqms_clean$flag_qc == "NON_VALID"),
    
    sum(aqms_clean$flag_qc == "MISSING"),
    
    sum(aqms_clean$flag_tahun_1970),
    
    sum(aqms_clean$flag_timestamp_missing),
    
    sum(aqms_clean$flag_interval_invalid),
    
    sum(aqms_clean$flag_duplikat_waktu),
    
    sum(aqms_clean$flag_polutan_negatif),
    
    sum(aqms_clean$flag_humidity_invalid),
    
    sum(aqms_clean$flag_wind_speed_invalid),
    
    sum(aqms_clean$flag_wind_direction_invalid),
    
    nrow(aqms_analysis),
    
    nrow(aqms_analysis_no_duplicate)
  )
)

print(ringkasan_cleaning)


# ============================================================
# BAGIAN 11. RINGKASAN DATA AQMS PER STASIUN
# ============================================================

ringkasan_stasiun <- aqms_analysis %>%
  
  group_by(station_file) %>%
  
  summarise(
    
    jumlah_data_valid = n(),
    
    tanggal_awal = min(
      tanggal,
      na.rm = TRUE
    ),
    
    tanggal_akhir = max(
      tanggal,
      na.rm = TRUE
    ),
    
    jumlah_pm25_ug_m3 =
      sum(!is.na(pm25_ug_m3)),
    
    jumlah_pm25_ppm =
      sum(!is.na(pm25_ppm)),
    
    jumlah_so2_ug_m3 =
      sum(!is.na(so2_ug_m3)),
    
    jumlah_so2_ppm =
      sum(!is.na(so2_ppm)),
    
    jumlah_no2_ug_m3 =
      sum(!is.na(no2_ug_m3)),
    
    jumlah_no2_ppm =
      sum(!is.na(no2_ppm)),
    
    .groups = "drop"
  )

print(ringkasan_stasiun, n = Inf)


# ============================================================
# BAGIAN 12. MEMERIKSA HASIL KONVERSI DAN MISSING
# ============================================================

audit_konversi <- tibble(
  
  variabel = kolom_angka,
  
  missing_sebelum = map_int(
    kolom_angka,
    ~ sum(is.na(aqms_raw[[.x]]))
  ),
  
  missing_sesudah = map_int(
    kolom_angka,
    ~ sum(is.na(aqms_clean[[.x]]))
  )
) %>%
  
  mutate(
    
    tambahan_missing =
      missing_sesudah - missing_sebelum
  )

print(audit_konversi)


# ============================================================
# BAGIAN 13. EKSPOR DATA HASIL TAHAP 2
# ============================================================

# Tutup file CSV/Excel hasil sebelumnya jika sedang terbuka.

# 13A. Menyimpan seluruh data AQMS dengan flag

write_csv(
  aqms_clean,
  file.path(
    folder_output,
    "AQMS_Clean_Full.csv"
  )
)

# 13B. Menyimpan data utama QC VALID

write_csv(
  aqms_analysis,
  file.path(
    folder_output,
    "AQMS_Analysis_Valid.csv"
  )
)

# 13C. Menyimpan data sensitivitas tanpa duplikat

write_csv(
  aqms_analysis_no_duplicate,
  file.path(
    folder_output,
    "AQMS_Analysis_No_Duplicate.csv"
  )
)

# 13D. Menyimpan data Health

write_csv(
  health_clean,
  file.path(
    folder_output,
    "Health_Clean.csv"
  )
)


# ============================================================
# BAGIAN 14. EKSPOR RINGKASAN TAHAP 2 KE EXCEL
# ============================================================

hasil_tahap2 <- list(
  
  "Ringkasan_Cleaning" =
    ringkasan_cleaning,
  
  "Ringkasan_Stasiun" =
    ringkasan_stasiun,
  
  "Ringkasan_Timestamp" =
    ringkasan_timestamp,
  
  "Ringkasan_Duplikat" =
    ringkasan_duplikat,
  
  "Ringkasan_QC" =
    ringkasan_qc,
  
  "Ringkasan_Nilai" =
    ringkasan_nilai,
  
  "Ringkasan_Health" =
    ringkasan_health,
  
  "Kamus_Health_Revisi" =
    kamus_health_revisi,
  
  "Audit_Konversi" =
    audit_konversi
)

write_xlsx(
  hasil_tahap2,
  file.path(
    folder_output,
    "Hasil_Preprocessing_Gelar_Rasa_2026.xlsx"
  )
)

