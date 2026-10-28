# Join Footywire draftee names to TORP player names.
#
# Footywire uses formal first names (Nikolas Cox, Lachlan Ash) where the AFL
# feed often uses the familiar one (Nik Cox, Lachie Ash), and capitalisation
# differs (Jacob Van Rooyen / van Rooyen). A surname-plus-initial match alone
# is wrong too often (Jacob Gillbee is not Jason Gillbee, Lachlan Jones is not
# Liam Jones), so a different first name is accepted only when the pair is a
# known short/long form below. Names still ambiguous after that are dropped.

.FIRST_NAME_FORMS <- list(
  c("lachlan", "lachie"), c("thomas", "tom"), c("mitchell", "mitch"),
  c("mitchito", "mitch"), c("matthew", "matt"), c("joshua", "josh"),
  c("zachary", "zach", "zac"), c("nicholas", "nick"), c("nikolas", "nik"),
  c("edward", "ed"), c("daniel", "dan"), c("samuel", "sam"), c("timothy", "tim"),
  c("william", "will"), c("cameron", "cam"), c("harrison", "harry"),
  c("patrick", "paddy"), c("dominic", "dom"), c("oliver", "ollie"),
  c("jackson", "jack"), c("joseph", "joe"), c("leonardo", "leo")
)

.first_key <- function(first) {
  first <- tolower(first)
  key <- first
  # Key on the shortest form: "mitch" belongs to both Mitchell and Mitchito,
  # so keying on the long form would let the later group overwrite the first.
  for (forms in .FIRST_NAME_FORMS) key[first %in% forms] <- forms[which.min(nchar(forms))]
  key
}

.name_key <- function(name) {
  name <- tolower(trimws(name))
  first <- sub(" .*$", "", name)
  surname <- sub("^[^ ]+ +", "", name)
  paste(.first_key(first), surname)
}

# drafts: Footywire rows with `player`; players: data.table with player_id and
# player_name. Returns drafts with player_id added (NA when no unique match).
match_draft_names <- function(drafts, players) {
  pl <- unique(data.table::as.data.table(players)[, .(player_id, player_name)])
  pl[, key := .name_key(player_name)]
  uniq <- pl[, .(player_id = if (data.table::uniqueN(player_id) == 1) player_id[1] else NA_character_),
             by = key]
  out <- data.table::copy(data.table::as.data.table(drafts))
  out[, key := .name_key(player)]
  out <- merge(out, uniq, by = "key", all.x = TRUE, sort = FALSE)
  out[, key := NULL]
  # Two different draft names landing on one player (Jackson Ross and Jack
  # Ross share a key) means at most one of them is him: keep the one whose
  # name matches exactly, ignoring case, and unmatch the rest.
  clash <- out[!is.na(player_id), .(n = data.table::uniqueN(tolower(player))), by = player_id][n > 1, player_id]
  if (length(clash)) {
    exact <- pl[player_id %in% clash, .(player_id, nm = tolower(player_name))]
    out[player_id %in% clash & !paste(player_id, tolower(player)) %in% paste(exact$player_id, exact$nm),
        player_id := NA_character_]
  }
  out
}
