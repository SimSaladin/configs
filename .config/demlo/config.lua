-- Demlo configuration
-- {{{ Docs
-- File:        <url:~/.config/demlo/config.lua>
-- User:        <url:~/.config/demlo/scripts>
--              <url:~/.config/demlo/actions>
-- System:      <url:/usr/share/demlo/config.lua>
--              <url:/usr/share/demlo/scripts>
--              <url:/usr/share/demlo/actions>
-- Help:        <url:vimscript:!demlo --help | less>
--              also: demlo -h [script-name]
--              <url:https://ambrevar.xyz/demlo/>
--[[
   Usage:
        -i idx       use index file for input/output data
        -o idx       write index to output file
        -t           fetch tags from internet
        -c           fetch cover from internet
        -p           APPLY CHANGES
        -r ''        remove all scripts
        -s '…'       add some scripts
        -pre lua     run before scripts
        -post lua    run after scripts
        -exist act   what do when destination exists

          demlo -t -r path -s remove_source album/*
                Change tags in-place with entries from MusicBrainz.

          demlo -ext webm -pre 'output.format="webm"' audio.webm
                Add support for non-default formats from commandline.

          demlo -exist writenewer audio.file
                Overwrite existing destination if input is newer

-- XXX :This when various artists is not indicated by tags.
--      -pre 'o.album_artist="various artists"'
}}} --]]
-- XXX: Remove leading track number from titles
--      -pre="sub={{[[^\d\d ]],''}}"

-- {{{ Core settings
Process = false -- If false, show preview and exit before processing.
Color = true
Cores = 0
Getcover = false
Gettags = false

Exist = ''

-- Extensions to look for when a folder is browsed.
-- The following variable is a map which keys are the extensions and the values are 'true'.
Extensions = {}
ext = {'aac', 'ape', 'flac', 'm4a', 'mid', 'mp2', 'mp3', 'mp4', 'mpg', 'mpga', 'mpc',
        'vob', 'mov', 'flv', 'mkv', 'ogg', 'wav', 'wv', 'wma', 'tta', 'swf'}
for _, v in ipairs(ext) do
	Extensions[v]=true
end

Prescript = '' -- -pre '…'
Postscript = '' -- -post '…'

-- }}}

-- {{{ Default scripts
--
-- Use global tables to pass data to/from scripts.
-- Global "input" is populated before first script.
-- global "output" is interpreted after scripts have run.
--
--      output.tags[album,performer,artist,album_artist, ...]
--
Scripts = {
        '10-tag-normalize', -- demlo script
        -- Removes tags it doesn't know about.  output.tags = tags o = output.tags
	--      demlo -pre 'o.artist=o.composer; o.title=o.artist .. " - " .. o.title' ...

        --'15-tag-disc_from_path', -- demlo script
        -- Gets disc number from a single digit of parent folder.
        --      demlo -pre 'o.track=input.path:match([[.*\/\D*(\d*)\D*]])' ...
        -- XXX: sometimes tries to be too clever

        '20-tag-replace', -- demlo script
        -- Straightforward search-and-replace among all tags.
        --      sub: substitutions

        '30-tag-case',  -- demlo script
        -- Sets case in tags to title case (The Quick Brown...) or sentence
        -- case (The quick brown...).
        --      scase: boolean (false)
        --      const: array of strings (to keep cased as specified)

        '40-tag-punctuation',  -- demlo script
        -- English punctuation rules. (No space before mark, one space after.)

        '50-encoding',  -- demlo script
        -- format/codec
        --
        -- output.format=input.format; output.parameters {'-c:a', 'copy'}
        --      With these settings taglib is used instead of ffmpeg.
        --      Put in post to disable encoding.
        --
        -- -pre bps=192000
        --      sets bitrate to 192k
        --
        -- -pre 'output.format="flac"'
        --      always re-encode to flac

        '60-path',  -- demlo script
        -- Sets output.path = {lib}
        --      /{album_artist}/{date}. {album} - Disc {disc}
        --      /{track}. {artist_ifelse} - {title}.{ext}
        --
        -- -pre "lib=\"$XDG_DIR_MUSIC\"'
        --      Set target directory for everything.
        --
        -- -pre 'pathsub={{[=[["*?:/\|<>]]=], ""}}'
        --      Avoids some special characeters, "*?:/\|<>, in filenames .

        '70-cover',  -- demlo script
        -- Cover sanitize:
        --      1. Remove any embedded covers/artwork.
        --      2. Convert to JPEG XXX.
        --      3. Skip too low quality covers.
        --      4. Skip duplicates.
        --
        -- If no output path is set for cover, it's skipped.
        -- If no target codec/format, covers are copied.
        -- If codec/format set, covers are transcoded to that.
        -- External covers are queried from file folder with known extensions.
        -- embedded covers are queried from files themselves.
        --
        -- Inputs:
        --      input.embedddedCovers { idx        = inputcover }
        --      input.externalcovers  { 'basename' = inputcover }
        --      input.onlinecover     inputcover
        --
        --      'inputcover'    { format =, width=, heigh=, checksum= }
        --      'format'        gif, jpeg, png
        --
        -- Outputs:
        --      output.embeddedcovers { idx = outputcover }
        --      output.externalcovers { 'basename' = outputcover }
        --      output.onlinecover    outputcover
        --
        --      outputcover { path='', format, parameters={} }
        --      parameters ( analogous to output.parameters )
        --
        -- demlo -p -c -r '' -s cover -s remove_source album/track
        --      DL cover only for album that corresponds to the track.
        --      remove_source.lua takes care file isn't duplicated.
        --

        '90-remove_source'  -- demlo script
        -- mv input output. XXX don't use lightly! is destrcutive operation.
        --
        --      output.removesource = true
        --      Really only sets this boolean: demlo's internals perform the
        --      actual rename/copy/removal.
}
-- }}}
