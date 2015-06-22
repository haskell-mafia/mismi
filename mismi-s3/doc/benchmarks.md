# Performance

## Download

# Single
| Size | Duration | app |
|------|----------|-----|
| 7.5 gb | 1m33.736s | aws cli |
| 7.5 gb | 3m13.230s | mismi |

# Multipart
| Size | Duration | app |
|------|----------|-----|
| 1 gb | 0m8s | aws cli |
| 1 gb | 0m24s | mismi (single) |
| 1 gb | 0m18s | mismi (chunk=100, fork=10) |


## Upload

| Size | Duration | app |
|------|----------|-----|
| 7.5 gb | 1m54.723s | aws cli |
| 7.5 gb | 1m36.056s | mismi |
| 23 gb | 4m52.645s | aws cli |
| 23 gb | 4m52.501s | aws cli |
| 23 gb | 8m31.622s | mismi - conduit (1mb buffer) |
| 23 gb | 4m58.076s | mismi - conduit (1mb buffer) |
| 23 gb | 4m52.799s | mismi - conduit (1mb buffer) |
| 23 gb | 11m28.957s | mismi - crazy (malloc) (4k buffer) |
| 23 gb | 4m51.605s | mismi - crazy (malloc) (1mb buffer) |
| 23 gb | 5m30.117s | mismi - crazy (malloc) (1mb buffer) |
