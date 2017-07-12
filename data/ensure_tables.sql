CREATE TABLE IF NOT EXISTS program (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE);
CREATE TABLE IF NOT EXISTS label (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE);
CREATE TABLE IF NOT EXISTS package (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE );
CREATE TABLE IF NOT EXISTS sample_file (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE );

CREATE TABLE IF NOT EXISTS sample
(id INTEGER PRIMARY KEY
, program_id INT NOT NULL REFERENCES program(id)
, label_id INT NOT NULL REFERENCES label(id)
, package_id INT NOT NULL REFERENCES package(id)
, sample_file_id INT NOT NULL REFERENCES sample_file(id)
, pwd TEXT NOT NULL
, command_line TEXT
, bytes_allocated INT NOT NULL
, num_GCs INT NOT NULL
, average_bytes_used INT NOT NULL
, max_bytes_used INT NOT NULL
, num_byte_usage_samples INT NOT NULL
, peak_megabytes_allocated INT NOT NULL
, init_cpu_seconds DOUBLE NOT NULL
, init_wall_seconds DOUBLE NOT NULL
, mutator_cpu_seconds DOUBLE NOT NULL
, mutator_wall_seconds DOUBLE NOT NULL
, GC_cpu_seconds DOUBLE NOT NULL
, GC_wall_seconds DOUBLE NOT NULL
);
