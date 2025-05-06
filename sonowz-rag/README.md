# Sonowz-RAG

RAG service intended for MCP endpoint

## TODO
- Add endpoint for uploading documents
- Knowledge extraction from LibreChat conversations
  - Extract personal information (knowledge level, personality, and preferences)
  - Information which LLM cannot know in prior (niche or recent)
  - Before storing, prompt top 10 items which seems too sensitive to store
- Indices to add
  - domain tag (Programming, Personal, Music, Finance, etc.)
  - date (extracted from document content)
  - importance (1-5), retrival frequency in a day & a year
    - retrieval frequency in a year is needed for removing old, useless document
  - relationship? (graph database)
