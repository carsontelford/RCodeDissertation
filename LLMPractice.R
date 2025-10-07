#### LLM in R ####

library(openai)
library(dplyr)
library(text2vec)
library(stringr)

# 1) Set API key
Sys.setenv(OPENAI_API_KEY = "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")

# 2) RAG input information
docs <- c(
  "Patient has a history of diabetes and hypertension. Treatment includes insulin and regular blood pressure checks. Patients should monitor their diet and exercise regularly.",
  "Asthma management requires inhalers and avoiding triggers. Cardiac patients benefit from exercise and low-salt diets. Regular check-ups are recommended for all patients."
)

# 3) Chunking function
chunk_doc <- function(text, chunk_size = 20, overlap = 10) {
  words <- unlist(str_split(text, "\\s+"))
  chunks <- list()
  start <- 1
  while (start <= length(words)) {
    end <- min(start + chunk_size - 1, length(words))
    chunk <- paste(words[start:end], collapse = " ")
    chunks <- c(chunks, chunk)
    start <- start + chunk_size - overlap
  }
  return(chunks)
}

# 4) Chunk all documents
chunked_docs <- unlist(lapply(docs, chunk_doc))
length(chunked_docs)  # number of chunks

# 5) Generate embeddings for each chunk
chunk_embeddings <- lapply(chunked_docs, function(txt) {
  res <- create_embedding(model = "text-embedding-3-small", input = txt)
  unlist(res$data[[1]]$embedding)
})
chunk_embeddings <- do.call(rbind, chunk_embeddings)
rownames(chunk_embeddings) <- paste0("chunk", 1:length(chunked_docs))

# 6) Query + embedding
query <- "What is the recommended treatment for hypertension?"
query_res <- create_embedding(model = "text-embedding-3-small", input = query)
query_emb <- unlist(query_res$data[[1]]$embedding)

# 7) Cosine similarity function
cosine_sim <- function(a, b) sum(a*b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

# 8) Retrieve top-k most similar RAG chunks to query
sims <- apply(chunk_embeddings, 1, cosine_sim, b=query_emb)
k <- 3
top_chunks <- chunked_docs[order(sims, decreasing = TRUE)][1:k]

# 9) Build LLM prompt with retrieved context
context <- paste(top_chunks, collapse = " ")
prompt <- paste0(
  "You are a helpful medical assistant.\n",
  "Question: ", query, "\n",
  "Context: ", context, "\n",
  "Answer the question using only the context above."
)

# 10) Call OpenAI Chat Completion
res <- create_chat_completion(
  model = "gpt-4o-mini",
  messages = list(
    list(role = "system", content = "You are a medical assistant."),
    list(role = "user", content = prompt)
  )
)

# 11) Print response
cat(res$choices[[1]]$message$content)











