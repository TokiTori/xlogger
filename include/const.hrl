%% Shared
-define(DEFAULT_WRITE_BLOCK_SIZE, 1024*4).
-define(DEFAULT_WRITE_DELAY, 500).

% handler.erl
-define(DEFAULT_FILE_NAME, "%YYYY-%MM-%DD/%level.log").
-define(DEFAULT_FILE_SIZE_LIMIT, 1024*1024*6).

% file_backend.erl
-define(FD_TIMEOUT, 30*1000).
-define(FD_EXPIRATION_CHECK_TIMEOUT, 10*1000).