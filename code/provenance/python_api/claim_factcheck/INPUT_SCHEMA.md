# Input Schema

The toolkit expects a long-format CSV at `data/messages.csv`.

## Required Columns

- `conversation_id`
  A stable ID for the conversation, participant, or dialogue unit.
- `message_id`
  A stable unique ID for each message row.
- `role`
  Usually `assistant` or `user`. The scripts process only `assistant` rows.
- `content`
  Plaintext message content.

## Recommended Columns

- `turn_index`
  Integer turn number within a conversation.
- `condition`
  Experimental condition or arm label.
- `study_id`
  Study, wave, or project label.
- `participant_id`
  If different from `conversation_id`.
- `message_timestamp`
  ISO-ish timestamp if available.
- `topic`
  Optional topic label.

## Example

See `templates/messages_template.csv`.

## One Good Rule

One row should equal one assistant message.

Do not:

- stuff multiple assistant turns into a single row unless that is intentionally
  your analysis unit,
- put the whole conversation in `content`,
- omit `role` and hope the scripts infer which rows are assistant outputs.

## If Your Raw Data Is Wide

Transform it before running the toolkit.

Typical wide-to-long cases:

- `assistant_response_1`, `assistant_response_2`, `assistant_response_3`
- one row per participant, several model-response columns
- one JSON transcript blob per participant

The transformation goal is always the same:

- one assistant message per row,
- stable conversation ID,
- stable message ID,
- clean plaintext content.
