import sqlite3
import openai
import os
from concurrent.futures import ThreadPoolExecutor, as_completed
from tqdm import tqdm

# Initialize DeepSeek API client
openai.api_key = 'sk-416aab8b28ca46dba7afadd0c85fb921'
openai.api_base = 'https://api.deepseek.com/v1'

# Connect to the prompts database
conn_prompts = sqlite3.connect('/Users/ethan/Desktop/CMSC 436 (Data Viz)/Project/Responses/deepseek-responses.db')
cursor_prompts = conn_prompts.cursor()

# Fetch prompts
# Batch 1: ids 1-1935
# Batch 2: ids 1936-1978
# Batch 3: ids 1979-3233
# Batch 4: ids 3234-3625
# 5: 3626-?
cursor_prompts.execute("SELECT id, prompt FROM prompts WHERE id >= 3716")
prompts = cursor_prompts.fetchall()

# Ensure the responses table exists
cursor_prompts.execute('''
CREATE TABLE IF NOT EXISTS responses (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    prompt_id INTEGER NOT NULL,
    prompt TEXT NOT NULL,
    response TEXT NOT NULL,
    FOREIGN KEY (prompt_id) REFERENCES prompts(id)
);''')
conn_prompts.commit()

# Function to process a single prompt
def process_prompt(prompt):
    prompt_id, prompt_text = prompt
    # Send prompt to DeepSeek API
    response = openai.ChatCompletion.create(
        model="deepseek-chat",
        messages=[
            {"role": "system", "content": "You are a helpful assistant."},
            {"role": "user", "content": prompt_text}
        ]
    )
    # Extract response text
    response_text = response.choices[0].message['content'].strip()
    return (prompt_id, prompt_text, response_text)

# Process prompts in parallel with a progress bar
with ThreadPoolExecutor(max_workers=16) as executor:
    futures = {executor.submit(process_prompt, prompt): prompt for prompt in prompts}
    for future in tqdm(as_completed(futures), total=len(prompts), desc="Processing prompts", unit="prompt"):
        prompt_id, prompt_text, response_text = future.result()
        if response_text:
            # Insert prompt and response into the responses database
            cursor_prompts.execute(
                "INSERT INTO responses (prompt_id, prompt, response) VALUES (?, ?, ?)",
                (prompt_id, prompt_text, response_text)
            )
            conn_prompts.commit()

# Close database connection
conn_prompts.close()
