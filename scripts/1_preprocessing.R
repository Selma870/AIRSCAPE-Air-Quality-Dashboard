
# ============================================================
# FINAL DATA AUDIT - PERBAIKAN DAN EKSPOR EXCEL
# ============================================================


# ============================================================
# 1. AUDIT MISSING VALUE BERDASARKAN SATUAN STASIUN
# ============================================================

missing_unit_aware <- map_dfr(
  names(data_aqms_list),
  
  function(nama_stasiun) {
    
    data <- data_aqms_list[[nama_stasiun]]
    
    kolom <- names(data)
    
    kolom_polutan <- kolom[
      str_detect(
        kolom,
        "^(pm25|pm10|so2|co|o3|no2)_(ug_m3|ppm)$"
      )
    ]
    
    jumlah_data <- nrow(data)
    
    jumlah_variabel <- length(kolom_polutan)
    
    total_missing <- sum(
      is.na(data[kolom_polutan])
    )
    
    total_sel <- jumlah_data * jumlah_variabel
    
    tibble(
      stasiun = nama_stasiun,
      jumlah_data = jumlah_data,
      jumlah_variabel_polutan = jumlah_variabel,
      total_missing = total_missing,
      total_sel = total_sel,
      
      persentase_missing = if (
        total_sel > 0
      ) {
        total_missing / total_sel * 100
      } else {
        NA_real_
      }
    )
  }
)

print(missing_unit_aware, n = Inf)


# ============================================================
# 2. AUDIT SATUAN POLUTAN
# ============================================================

audit_satuan_baru <- map_dfr(
  names(data_aqms_list),
  
  function(nama_stasiun) {
    
    kolom <- names(
      data_aqms_list[[nama_stasiun]]
    )
    
    jumlah_ug <- sum(
      str_detect(kolom, "ug_m3")
    )
    
    jumlah_ppm <- sum(
      str_detect(kolom, "ppm")
    )
    
    tibble(
      stasiun = nama_stasiun,
      jumlah_kolom_ug_m3 = jumlah_ug,
      jumlah_kolom_ppm = jumlah_ppm,
      
      jenis_satuan = case_when(
        jumlah_ug > 0 & jumlah_ppm > 0 ~ "Keduanya",
        jumlah_ug > 0 ~ "ug/m3",
        jumlah_ppm > 0 ~ "ppm",
        TRUE ~ "Tidak teridentifikasi"
      )
    )
  }
)

print(audit_satuan_baru, n = Inf)


# ============================================================
# 3. PEMERIKSAAN TIMESTAMP MUARO JAMBI
# ============================================================

# Mengambil data Muaro Jambi dari data hasil konversi

muaro <- aqms %>%
  filter(
    str_detect(station_file, "MuaroJambi")
  )

# Mengambil data mentah dengan urutan baris yang sama

indeks_muaro <- which(
  str_detect(aqms_raw$station_file, "MuaroJambi")
)

muaro_raw <- aqms_raw[indeks_muaro, ]

# Menemukan timestamp yang terbaca tahun 1970

indeks_1970 <- which(
  !is.na(muaro$timestamp_start) &
    year(muaro$timestamp_start) == 1970
)

# Menggabungkan informasi timestamp mentah dan hasil parsing

if (length(indeks_1970) > 0) {
  
  muaro_1970 <- tibble(
    measurement_id =
      muaro_raw$measurement_id[indeks_1970],
    
    timestamp_start_raw =
      muaro_raw$timestamp_start[indeks_1970],
    
    timestamp_end_raw =
      muaro_raw$timestamp_end[indeks_1970],
    
    timestamp_start_parsed =
      muaro$timestamp_start[indeks_1970],
    
    timestamp_end_parsed =
      muaro$timestamp_end[indeks_1970]
  )
  
} else {
  
  muaro_1970 <- tibble(
    keterangan = "Tidak ditemukan timestamp tahun 1970"
  )
}

print(muaro_1970, n = 30)


# ============================================================
# 4. MEMBUAT RINGKASAN MASALAH TERBARU
# ============================================================

catatan_masalah_baru <- tibble(
  
  jenis_pemeriksaan = c(
    "Total file CSV",
    "Total file AQMS",
    "Total baris AQMS",
    "Total baris Health",
    "Total missing AQMS",
    "Total missing Health",
    "Jumlah duplikat ID",
    "Jumlah duplikat waktu",
    "Jumlah stasiun diaudit QC",
    "Stasiun menggunakan ug/m3",
    "Stasiun menggunakan ppm",
    "Total outlier terdeteksi",
    "Jumlah timestamp Muaro Jambi tahun 1970"
  ),
  
  hasil = c(
    length(semua_file),
    length(file_aqms),
    nrow(aqms_raw),
    nrow(health_raw),
    sum(is.na(aqms_raw)),
    sum(is.na(health_raw)),
    nrow(duplikat_id),
    nrow(duplikat_waktu),
    nrow(audit_qc),
    sum(audit_satuan_baru$jenis_satuan == "ug/m3"),
    sum(audit_satuan_baru$jenis_satuan == "ppm"),
    sum(audit_outlier$jumlah_outlier),
    length(indeks_1970)
  )
)

print(catatan_masalah_baru)


# ============================================================
# 5. EKSPOR ULANG SELURUH HASIL AUDIT KE EXCEL
# ============================================================

hasil_audit_final <- list(
  
  "Dictionary_AQMS" = dictionary_aqms,
  
  "Dictionary_Health" = dictionary_health,
  
  "Audit_Seluruh_File" = tabel_audit,
  
  "Missing_AQMS" = missing_aqms,
  
  "Missing_Health" = missing_health,
  
  "Missing_Per_Stasiun" = missing_per_stasiun,
  
  "Missing_Sesuai_Satuan" = missing_unit_aware,
  
  "Duplikat_ID" = duplikat_id,
  
  "Duplikat_Waktu" = duplikat_waktu,
  
  "Audit_Waktu" = audit_waktu,
  
  "Audit_QC" = audit_qc,
  
  "Audit_Satuan_Lama" = audit_satuan,
  
  "Audit_Satuan_Final" = audit_satuan_baru,
  
  "Audit_Outlier" = audit_outlier,
  
  "Timestamp_Muaro_1970" = muaro_1970,
  
  "Ringkasan_Masalah_Final" = catatan_masalah_baru
)

# Menyimpan file Excel versi terbaru

write_xlsx(
  hasil_audit_final,
  
  path = file.path(
    folder_output,
    "Hasil_Data_Audit_Gelar_Rasa_2026.xlsx"
  )
)

cat(
  "\n====================================\n",
  "FINAL DATA AUDIT BERHASIL DIEKSPOR!\n",
  "====================================\n",
  "\nLokasi file:\n",
  file.path(
    folder_output,
    "Hasil_Data_Audit_Gelar_Rasa_2026.xlsx"
  )
)

aqms <- aqms %>%
  mutate(
    flag_timestamp = case_when(
      year(timestamp_start) == 1970 ~
        "Anomali timestamp sumber",
      
      is.na(timestamp_start) ~
        "Timestamp tidak terbaca",
      
      TRUE ~
        "Timestamp normal"
    )
  )

# Menghitung jumlah setiap kategori

table(aqms$flag_timestamp)


# ============
# NEW
# ============

# ============================================================
# 1. MEMBUAT TABEL RINGKASAN TIMESTAMP
# ============================================================

audit_flag_timestamp <- aqms %>%
  count(
    flag_timestamp,
    name = "jumlah_data"
  ) %>%
  mutate(
    persentase = jumlah_data / sum(jumlah_data) * 100
  )

print(audit_flag_timestamp)


# ============================================================
# 2. MEMBUAT TABEL DETAIL ANOMALI TIMESTAMP
# ============================================================

detail_anomali_timestamp <- aqms %>%
  filter(
    flag_timestamp == "Anomali timestamp sumber"
  ) %>%
  select(
    station_file,
    measurement_id,
    sensor_id,
    timestamp_start,
    timestamp_end,
    flag_timestamp
  )

print(detail_anomali_timestamp)


# ============================================================
# 3. MEMPERBARUI RINGKASAN MASALAH
# ============================================================

catatan_masalah_baru <- catatan_masalah_baru %>%
  filter(
    jenis_pemeriksaan !=
      "Jumlah timestamp Muaro Jambi tahun 1970"
  ) %>%
  bind_rows(
    tibble(
      jenis_pemeriksaan =
        "Jumlah timestamp anomali tahun 1970",
      
      hasil = sum(
        aqms$flag_timestamp ==
          "Anomali timestamp sumber",
        na.rm = TRUE
      )
    )
  )


# ============================================================
# 4. MENAMBAHKAN SHEET BARU KE EXCEL
# ============================================================

hasil_audit_final[["Audit_Flag_Timestamp"]] <-
  audit_flag_timestamp

hasil_audit_final[["Detail_Anomali_Timestamp"]] <-
  detail_anomali_timestamp

hasil_audit_final[["Ringkasan_Masalah_Final"]] <-
  catatan_masalah_baru


# ============================================================
# 5. EKSPOR KE FILE EXCEL TERBARU
# ============================================================

write_xlsx(
  hasil_audit_final,
  path = file.path(
    folder_output,
    "Hasil_Data_Audit_Gelar_Rasa_2026_FINAL.xlsx"
  )
)

cat("Excel audit berhasil diperbarui!")


# ============================================================
# DATA AUDIT
# Tujuan:
# 1. Membaca seluruh file dataset
# 2. Membuat data dictionary
# 3. Audit setiap file
# 4. Memeriksa missing values
# 5. Memeriksa data duplikat
# 6. Memeriksa format tanggal dan waktu
# 7. Memeriksa QC setiap stasiun
# 8. Memeriksa perbedaan satuan
# 9. Mengidentifikasi nilai ekstrem (outlier)
# 10. Mengekspor hasil audit ke Excel
# ============================================================


# ============================================================
# BAGIAN 1. INSTALL DAN MEMANGGIL PACKAGE
# ============================================================

# Jalankan install.packages() satu kali saja jika belum terinstal

install.packages(c(
  "janitor",
  "writexl"
))

# Memanggil package

library(tidyverse)
library(lubridate)
library(readr)
library(janitor)
library(writexl)
library(stringr)


# ============================================================
# BAGIAN 2. MENENTUKAN FOLDER DATASET
# ============================================================

# Pilih folder hasil ekstrak ZIP dataset Gelar Rasa 2026

folder_data <- choose.dir(
  caption = "C:/Users/Portege/Downloads/Dataset Gelar Rasa"
)

# Periksa apakah folder berhasil dipilih

if (is.na(folder_data)) {
  stop("Folder dataset belum dipilih!")
}

# Membuat folder khusus untuk menyimpan hasil audit

folder_output <- file.path(folder_data, "HASIL_DATA_AUDIT")

if (!dir.exists(folder_output)) {
  dir.create(folder_output)
}

# Melihat lokasi folder dataset

folder_data
folder_output


# ============================================================
# BAGIAN 3. MEMBACA SELURUH FILE AQMS
# ============================================================

# Mencari seluruh file AQMS

file_aqms <- list.files(
  path = folder_data,
  pattern = "_AQMS\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Melihat jumlah file AQMS

length(file_aqms)

# Membaca seluruh file AQMS

data_aqms_list <- map(
  file_aqms,
  ~ read_csv(.x, show_col_types = FALSE)
)

# Memberikan nama sesuai nama file

names(data_aqms_list) <- tools::file_path_sans_ext(
  basename(file_aqms)
)

# Menggabungkan seluruh data AQMS

aqms_raw <- imap_dfr(
  data_aqms_list,
  ~ mutate(.x, station_file = .y)
)

# Melihat struktur data

dim(aqms_raw)
glimpse(aqms_raw)
head(aqms_raw)

# Melihat nama seluruh kolom

names(aqms_raw)


# ============================================================
# BAGIAN 4. MEMBACA DATA HEALTH OUTCOMES
# ============================================================

# Mencari file health outcomes

file_health <- list.files(
  path = folder_data,
  pattern = "health_outcomes_daily\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Memeriksa apakah file ditemukan

if (length(file_health) == 0) {
  stop("File health_outcomes_daily.csv tidak ditemukan!")
}

# Membaca data health

health_raw <- read_csv(
  file_health[1],
  show_col_types = FALSE
)

# Melihat struktur data health

dim(health_raw)
glimpse(health_raw)
head(health_raw)


# ============================================================
# BAGIAN 5. DATA DICTIONARY AQMS
# ============================================================

# Data dictionary menjelaskan:
# - Nama variabel
# - Jenis data
# - Jumlah missing value
# - Jumlah nilai unik

dictionary_aqms <- tibble(
  nama_variabel = names(aqms_raw),
  
  tipe_data = map_chr(
    aqms_raw,
    ~ paste(class(.x), collapse = ", ")
  ),
  
  jumlah_missing = map_int(
    aqms_raw,
    ~ sum(is.na(.x))
  ),
  
  persentase_missing = map_dbl(
    aqms_raw,
    ~ mean(is.na(.x)) * 100
  ),
  
  jumlah_nilai_unik = map_int(
    aqms_raw,
    ~ n_distinct(.x, na.rm = TRUE)
  )
)

# Melihat hasil

print(dictionary_aqms)


# ============================================================
# BAGIAN 6. DATA DICTIONARY HEALTH
# ============================================================

dictionary_health <- tibble(
  nama_variabel = names(health_raw),
  
  tipe_data = map_chr(
    health_raw,
    ~ paste(class(.x), collapse = ", ")
  ),
  
  jumlah_missing = map_int(
    health_raw,
    ~ sum(is.na(.x))
  ),
  
  persentase_missing = map_dbl(
    health_raw,
    ~ mean(is.na(.x)) * 100
  ),
  
  jumlah_nilai_unik = map_int(
    health_raw,
    ~ n_distinct(.x, na.rm = TRUE)
  )
)

print(dictionary_health)


# ============================================================
# BAGIAN 7. AUDIT SELURUH FILE CSV
# ============================================================

# Mencari seluruh file CSV di dalam folder dataset

semua_file <- list.files(
  path = folder_data,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Fungsi untuk memeriksa satu file

audit_satu_file <- function(lokasi_file) {
  
  data <- read_csv(
    lokasi_file,
    show_col_types = FALSE
  )
  
  tibble(
    nama_file = basename(lokasi_file),
    
    jumlah_baris = nrow(data),
    
    jumlah_kolom = ncol(data),
    
    total_missing = sum(is.na(data)),
    
    persentase_missing =
      mean(is.na(data)) * 100
  )
}

# Menerapkan fungsi ke seluruh file CSV

tabel_audit <- map_dfr(
  semua_file,
  audit_satu_file
)

# Melihat hasil audit

print(tabel_audit)


# ============================================================
# BAGIAN 8. AUDIT MISSING VALUE AQMS
# ============================================================

missing_aqms <- aqms_raw %>%
  
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) %>%
  
  pivot_longer(
    cols = everything(),
    names_to = "nama_variabel",
    values_to = "jumlah_missing"
  ) %>%
  
  mutate(
    persentase_missing =
      jumlah_missing / nrow(aqms_raw) * 100
  ) %>%
  
  arrange(desc(jumlah_missing))

print(missing_aqms)


# ============================================================
# BAGIAN 9. AUDIT MISSING VALUE HEALTH
# ============================================================

missing_health <- health_raw %>%
  
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) %>%
  
  pivot_longer(
    cols = everything(),
    names_to = "nama_variabel",
    values_to = "jumlah_missing"
  ) %>%
  
  mutate(
    persentase_missing =
      jumlah_missing / nrow(health_raw) * 100
  ) %>%
  
  arrange(desc(jumlah_missing))

print(missing_health)


# ============================================================
# BAGIAN 10. MEMERIKSA DATA DUPLIKAT
# ============================================================

# 10A. Duplikat berdasarkan kombinasi stasiun dan measurement_id

duplikat_id <- aqms_raw %>%
  
  filter(!is.na(measurement_id)) %>%
  
  count(
    station_file,
    measurement_id,
    name = "jumlah"
  ) %>%
  
  filter(jumlah > 1) %>%
  
  arrange(desc(jumlah))

print(duplikat_id)


# 10B. Duplikat berdasarkan sensor dan timestamp

duplikat_waktu <- aqms_raw %>%
  
  filter(
    !is.na(sensor_id),
    !is.na(timestamp_start)
  ) %>%
  
  count(
    station_file,
    sensor_id,
    timestamp_start,
    name = "jumlah"
  ) %>%
  
  filter(jumlah > 1) %>%
  
  arrange(desc(jumlah))

print(duplikat_waktu)


# ============================================================
# BAGIAN 11. MEMERIKSA FORMAT TANGGAL DAN WAKTU
# ============================================================

# Mengubah timestamp menjadi format datetime
# Kolom asli tetap dipertahankan

aqms_raw <- aqms_raw %>%
  
  mutate(
    timestamp_start_parsed =
      ymd_hms(
        as.character(timestamp_start),
        quiet = TRUE
      ),
    
    timestamp_end_parsed =
      ymd_hms(
        as.character(timestamp_end),
        quiet = TRUE
      )
  )

# Ringkasan pemeriksaan waktu per stasiun

audit_waktu <- aqms_raw %>%
  
  group_by(station_file) %>%
  
  summarise(
    
    jumlah_data = n(),
    
    timestamp_start_missing =
      sum(is.na(timestamp_start_parsed)),
    
    timestamp_end_missing =
      sum(is.na(timestamp_end_parsed)),
    
    waktu_mulai_lebih_besar =
      sum(
        timestamp_start_parsed >
          timestamp_end_parsed,
        na.rm = TRUE
      ),
    
    waktu_awal =
      min(timestamp_start_parsed, na.rm = TRUE),
    
    waktu_akhir =
      max(timestamp_start_parsed, na.rm = TRUE),
    
    .groups = "drop"
  )

print(audit_waktu)


# ============================================================
# BAGIAN 12. MEMERIKSA QC SETIAP STASIUN
# ============================================================

# Melihat kategori QC yang tersedia

table(
  aqms_raw$qc_flag,
  useNA = "always"
)

# Menghitung jumlah data VALID dan non-VALID

audit_qc <- aqms_raw %>%
  
  group_by(station_file) %>%
  
  summarise(
    
    total_data = n(),
    
    jumlah_valid = sum(
      qc_flag == "VALID",
      na.rm = TRUE
    ),
    
    jumlah_non_valid = sum(
      !is.na(qc_flag) & qc_flag != "VALID"
    ),
    
    jumlah_qc_missing =
      sum(is.na(qc_flag)),
    
    persentase_valid =
      jumlah_valid / total_data * 100,
    
    persentase_non_valid =
      jumlah_non_valid / total_data * 100,
    
    .groups = "drop"
  ) %>%
  
  arrange(persentase_valid)

print(audit_qc)


# ============================================================
# BAGIAN 13. MEMERIKSA PERBEDAAN SATUAN POLUTAN
# ============================================================

# Mengidentifikasi kolom yang memiliki satuan ug/m3 atau ppm

audit_satuan <- map_dfr(
  names(data_aqms_list),
  
  function(nama_stasiun) {
    
    nama_kolom <- names(
      data_aqms_list[[nama_stasiun]]
    )
    
    kolom_ug <- nama_kolom[
      str_detect(
        nama_kolom,
        regex("ug_m3", ignore_case = TRUE)
      )
    ]
    
    kolom_ppm <- nama_kolom[
      str_detect(
        nama_kolom,
        regex("ppm", ignore_case = TRUE)
      )
    ]
    
    tibble(
      
      stasiun = nama_stasiun,
      
      kolom_ug_m3 = paste(
        kolom_ug,
        collapse = ", "
      ),
      
      kolom_ppm = paste(
        kolom_ppm,
        collapse = ", "
      ),
      
      jumlah_kolom_ug_m3 = length(kolom_ug),
      
      jumlah_kolom_ppm = length(kolom_ppm),
      
      indikasi_perbedaan_satuan =
        ifelse(
          length(kolom_ug) > 0 &
            length(kolom_ppm) > 0,
          "Ada kedua jenis satuan",
          ifelse(
            length(kolom_ppm) > 0,
            "Ada kolom ppm",
            ifelse(
              length(kolom_ug) > 0,
              "Ada kolom ug/m3",
              "Perlu diperiksa"
            )
          )
        )
    )
  }
)

print(audit_satuan)



# ============================================================
# BAGIAN 14. MEMERIKSA NILAI EKSTREM / OUTLIER
# ============================================================

# Memilih kolom polutan dan meteorologi

calon_kolom <- c(
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

# Memilih kolom yang benar-benar tersedia dan numerik

kolom_numerik <- calon_kolom[
  calon_kolom %in% names(aqms)
]

kolom_numerik <- kolom_numerik[
  map_lgl(
    aqms[kolom_numerik],
    ~ is.numeric(.x) && any(!is.na(.x))
  )
]

# Cek kolom yang akan dianalisis

print(kolom_numerik)

# Jika tidak ada kolom numerik, hentikan proses

if (length(kolom_numerik) == 0) {
  stop("Tidak ada kolom numerik yang dapat diperiksa!")
}

# Menghitung outlier menggunakan metode IQR


audit_outlier <- aqms %>%
  
  select(
    station_file,
    all_of(kolom_numerik)
  ) %>%
  
  pivot_longer(
    cols = all_of(kolom_numerik),
    names_to = "variabel",
    values_to = "nilai"
  ) %>%
  
  group_by(
    station_file,
    variabel
  ) %>%
  
  filter(!is.na(nilai)) %>%
  
  summarise(
    
    jumlah_data = n(),
    
    Q1 = quantile(nilai, 0.25),
    
    Q3 = quantile(nilai, 0.75),
    
    IQR = IQR(nilai),
    
    batas_bawah = Q1 - 1.5 * IQR,
    
    batas_atas = Q3 + 1.5 * IQR,
    
    jumlah_outlier = sum(
      nilai < batas_bawah |
        nilai > batas_atas
    ),
    
    persentase_outlier =
      jumlah_outlier / jumlah_data * 100,
    
    nilai_minimum = min(nilai),
    
    nilai_maksimum = max(nilai),
    
    .groups = "drop"
  )

# Periksa hasil

print(audit_outlier)


# ============================================================
# BAGIAN 15. RINGKASAN MASALAH DATA
# ============================================================

catatan_masalah <- tibble(
  
  jenis_pemeriksaan = c(
    "Total file CSV",
    "Total file AQMS",
    "Total baris AQMS",
    "Total baris Health",
    "Total missing value AQMS",
    "Total missing value Health",
    "Jumlah kombinasi ID duplikat",
    "Jumlah kombinasi waktu duplikat",
    "Jumlah stasiun yang diaudit QC",
    "Jumlah stasiun dengan kolom ppm",
    "Jumlah stasiun dengan kolom ug/m3",
    "Jumlah baris outlier yang terdeteksi"
  ),
  
  hasil = c(
    
    length(semua_file),
    
    length(file_aqms),
    
    nrow(aqms_raw),
    
    nrow(health_raw),
    
    sum(is.na(aqms_raw)),
    
    sum(is.na(health_raw)),
    
    nrow(duplikat_id),
    
    nrow(duplikat_waktu),
    
    nrow(audit_qc),
    
    sum(audit_satuan$jumlah_kolom_ppm > 0),
    
    sum(audit_satuan$jumlah_kolom_ug_m3 > 0),
    
    sum(audit_outlier$jumlah_outlier)
  )
)

print(catatan_masalah)


# ============================================================
# BAGIAN 16. EKSPOR HASIL AUDIT KE EXCEL
# ============================================================

hasil_audit <- list(
  
  "Dictionary_AQMS" = dictionary_aqms,
  
  "Dictionary_Health" = dictionary_health,
  
  "Audit_Seluruh_File" = tabel_audit,
  
  "Missing_AQMS" = missing_aqms,
  
  "Missing_Health" = missing_health,
  
  "Duplikat_ID" = duplikat_id,
  
  "Duplikat_Waktu" = duplikat_waktu,
  
  "Audit_Waktu" = audit_waktu,
  
  "Audit_QC" = audit_qc,
  
  "Audit_Satuan" = audit_satuan,
  
  "Audit_Outlier" = audit_outlier,
  
  "Ringkasan_Masalah" = catatan_masalah
)

# Menyimpan seluruh hasil ke satu file Excel

write_xlsx(
  
  hasil_audit,
  
  path = file.path(
    folder_output,
    "Hasil_Data_Audit_Gelar_Rasa_2026.xlsx"
  )
)

# Pesan jika proses selesai

cat(
  "\nDATA AUDIT SELESAI!\n",
  "Hasil tersimpan di folder:\n",
  folder_output
)


# ========================================
# 17. PERBAIKAN
# ========================================

# ============================================================
# PERBAIKAN AUDIT MISSING PER STASIUN
# ============================================================

kolom_polutan <- c(
  "pm25_ug_m3", "pm10_ug_m3",
  "so2_ug_m3", "co_ug_m3",
  "o3_ug_m3", "no2_ug_m3",
  "pm25_ppm", "pm10_ppm",
  "so2_ppm", "co_ppm",
  "o3_ppm", "no2_ppm"
)

kolom_polutan <- intersect(
  kolom_polutan,
  names(aqms)
)

missing_per_stasiun <- aqms %>%
  group_by(station_file) %>%
  summarise(
    across(
      all_of(kolom_polutan),
      ~ sum(is.na(.x)),
      .names = "missing_{.col}"
    ),
    jumlah_data = n(),
    .groups = "drop"
  )

print(missing_per_stasiun)


# ============================================================
# PERBAIKAN AUDIT SATUAN
# ============================================================

audit_satuan_baru <- map_dfr(
  names(data_aqms_list),
  
  function(nama_stasiun) {
    
    kolom <- names(data_aqms_list[[nama_stasiun]])
    
    jumlah_ug <- sum(
      str_detect(kolom, "ug_m3")
    )
    
    jumlah_ppm <- sum(
      str_detect(kolom, "ppm")
    )
    
    tibble(
      stasiun = nama_stasiun,
      jumlah_kolom_ug_m3 = jumlah_ug,
      jumlah_kolom_ppm = jumlah_ppm,
      jenis_satuan = case_when(
        jumlah_ug > 0 & jumlah_ppm > 0 ~ "Keduanya",
        jumlah_ug > 0 ~ "ug/m3",
        jumlah_ppm > 0 ~ "ppm",
        TRUE ~ "Tidak teridentifikasi"
      )
    )
  }
)

print(audit_satuan_baru)


# ============================================================
# MEMERIKSA TIMESTAMP MUARO JAMBI
# ============================================================

muaro <- aqms_raw %>%
  filter(
    str_detect(station_file, "MuaroJambi")
  )

# Melihat contoh timestamp mentah

head(
  muaro %>%
    select(
      timestamp_start,
      timestamp_end
    ),
  20
)

# Menemukan baris yang terbaca tahun 1970

muaro_1970 <- aqms %>%
  filter(
    str_detect(station_file, "MuaroJambi"),
    year(timestamp_start) == 1970
  ) %>%
  select(
    measurement_id,
    sensor_id,
    timestamp_start,
    timestamp_end
  )

print(muaro_1970)
