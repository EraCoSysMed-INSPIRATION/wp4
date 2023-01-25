work_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dat_old <- read.table(file.path(work_dir, "DATASET_DEXTROMETHORPHAN_V04.csv"), header = TRUE, sep = ",")
dat_new <- dat_old

dat_raw <- list()
mols <- c("DTT", "DXT")
dat_raw <- sapply(mols, function(x) {
    path_to_file <- file.path(work_dir, paste0("FR09_PO_30_", x, "_SD_PVB.csv"))
    dat <- read.table(path_to_file, header = TRUE, sep = ",")
    colnames(dat) <- c("ID", "PID", "TIME", "DV")
    dat_raw[[length(dat_raw) + 1]] <- dat
    return(dat_raw)
})

for (n in seq_along(dat_raw)) {
    # match id 
    keys <- c("A", "B", "C", "D", "E")
    dosing <- dat_old[dat_old$EVID == 1, ]

    for (id in unique(dosing$ID)) {
        # convert to key
        pid <- paste0(keys[[as.numeric(substr(as.character(id), 2, 2))]], substr(as.character(id), 3, 4))
        row <- dosing[dosing$ID == id, ]
        # add to data
        dat_pid <- dat_raw[[n]][dat_raw[[n]]$PID == pid, ]
        row_reps <- row[rep(seq_len(nrow(row)), each = nrow(dat_pid)), ]
        # add time and dv to row_reps
        for (col in c("TIME", "DV")) {
            row_reps[[col]] <- dat_pid[[col]]
        }
        row_reps$EVID <- 0
        row_reps$MDV <- 0
        row_reps$CMT <- 2 + n # 2 = DEX, 3 = DTT, 4 = DXT
        dat_new <- rbind(dat_new, row_reps)



    }
}

dat_new$TAD <- dat_new$TIME # wow
out <- dat_new[order(dat_new$ID, dat_new$TIME), ]
write.csv(out, file.path(work_dir, "DATASET_DEXTROMETHORPHAN_V05.csv"), row.names = FALSE)

