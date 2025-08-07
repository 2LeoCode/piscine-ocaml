let rec hfs_f = function 0 -> 1 | n -> n - hfs_m (hfs_f (n - 1))
and hfs_m = function 0 -> 0 | n -> n - hfs_f (hfs_m (n - 1))
