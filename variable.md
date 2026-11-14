# DATA DICTIONARY — AIRSCAPE

## 1. Informasi Dataset

| Informasi | Keterangan |
|---|---|
| Nama proyek | AIRSCAPE — Air Quality Monitoring Dashboard |
| Sumber data | Dataset Gelar Rasa 2026 |
| Periode pengamatan | 1 Januari 2024 – 14 Oktober 2026 |
| Jumlah stasiun AQMS | 27 stasiun |
| Jumlah parameter polutan | 6 parameter |
| Perangkat analisis | R dan RStudio |
| Platform visualisasi | Google Looker Studio |

## 2. Data Pemantauan Kualitas Udara (AQMS)

| Variabel | Deskripsi | Tipe Data | Satuan |
|---|---|---|---|
| measurement_id | Identitas pengukuran | Character | - |
| sensor_id | Identitas sensor | Character | - |
| timestamp_start | Waktu awal pengukuran | Datetime | - |
| timestamp_end | Waktu akhir pengukuran | Datetime | - |
| pm25_ug_m3 | Konsentrasi PM2.5 | Numeric | µg/m³ |
| pm10_ug_m3 | Konsentrasi PM10 | Numeric | µg/m³ |
| so2_ug_m3 | Konsentrasi SO2 | Numeric | µg/m³ |
| co_ug_m3 | Konsentrasi CO | Numeric | µg/m³ |
| o3_ug_m3 | Konsentrasi O3 | Numeric | µg/m³ |
| no2_ug_m3 | Konsentrasi NO2 | Numeric | µg/m³ |
| temperature_c | Suhu udara | Numeric | °C |
| humidity_pct | Kelembapan udara | Numeric | % |
| wind_speed_ms | Kecepatan angin | Numeric | m/s |
| wind_direction_deg | Arah angin | Numeric | Derajat |
| qc_flag | Status kualitas pengukuran | Character | - |
| stasiun | Nama stasiun pemantauan | Character | - |

**Catatan satuan:** Terdapat tiga stasiun dengan variabel polutan bersatuan ppm, yaitu Banjarbaru, Medan, dan Pesawaran. Nilai dengan satuan ppm tidak langsung dibandingkan dengan nilai µg/m³ tanpa harmonisasi satuan yang tervalidasi.

## 3. Data Kesehatan

| Variabel | Deskripsi | Tipe Data | Satuan |
|---|---|---|---|
| date | Tanggal pencatatan | Date | - |
| hospital_id | Identitas rumah sakit | Character | - |
| resp_admissions_under5 | Rawat inap respirasi usia di bawah 5 tahun | Numeric | Kasus |
| resp_admissions_over60 | Rawat inap respirasi usia di atas 60 tahun | Numeric | Kasus |
| asthma_emergency_cases | Kasus darurat asma | Numeric | Kasus |
| copd_admissions | Rawat inap PPOK | Numeric | Kasus |
| resp | Jumlah kasus respirasi | Numeric | Kasus |
| cvd | Jumlah kasus kardiovaskular | Numeric | Kasus |
| death | Jumlah kematian tercatat | Numeric | Kasus |

## 4. Metadata Stasiun

| Variabel | Deskripsi | Tipe Data |
|---|---|---|
| station_code | Kode stasiun AQMS | Character |
| location_id | Identitas lokasi | Character |
| provinsi | Provinsi lokasi stasiun | Character |
| kota_kabupaten | Kabupaten/kota lokasi stasiun | Character |
| kecamatan | Kecamatan lokasi stasiun | Character |
| latitude_dd | Koordinat lintang | Numeric |
| longitude_dd | Koordinat bujur | Numeric |
| baseline_pm25 | Nilai baseline PM2.5 | Numeric |
| region_group | Kelompok wilayah | Character |
| site_type | Jenis lokasi pemantauan | Character |
| distance_to_road_m | Jarak stasiun dari jalan | Numeric |
| traffic_intensity_per_h | Intensitas lalu lintas per jam | Numeric |

## 5. Variabel Hasil Pengolahan dan Analisis

| Variabel | Deskripsi | Tipe Data |
|---|---|---|
| tanggal | Tanggal agregasi pengamatan | Date |
| rata_rata_jaringan | Rata-rata konsentrasi PM2.5 jaringan | Numeric |
| median_jaringan | Median konsentrasi PM2.5 jaringan | Numeric |
| jumlah_stasiun | Jumlah stasiun dengan pengamatan valid | Integer |
| durasi_hari | Durasi episode PM2.5 | Integer |
| konsentrasi_puncak | Konsentrasi puncak episode | Numeric |
| median_puncak | Median konsentrasi episode | Numeric |
| persentase_lolos | Persentase data dengan QC VALID | Numeric |
| gap_terpanjang | Jeda terpanjang pengamatan valid | Numeric, jam |
| kategori_monitoring | Kategori evaluasi monitoring stasiun | Character |

## 6. Keterangan Preprocessing

1. Seleksi pengamatan berdasarkan status kualitas data (QC).
2. Pemeriksaan format waktu dan tanggal pengamatan.
3. Konversi variabel konsentrasi menjadi tipe numerik.
4. Pemeriksaan nilai hilang dan ketidaksesuaian satuan.
5. Agregasi pengamatan untuk analisis harian.
6. Pembentukan indikator episode PM2.5.
7. Evaluasi kelengkapan data dan jeda pengamatan stasiun.
8. Ekspor data hasil pengolahan untuk visualisasi dashboard.

## 7. Catatan Interpretasi

Hasil analisis menggambarkan pola dalam dataset yang digunakan. Hubungan antara konsentrasi polutan dan indikator kesehatan tidak secara langsung menunjukkan hubungan sebab-akibat.

Variabel dan satuan dalam dokumen ini perlu dicocokkan dengan struktur CSV final sebelum digunakan sebagai dokumentasi resmi.
