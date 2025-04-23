import re
from collections import defaultdict

# Output Clingo (letto da file)
try:
    with open("calendar_output.txt", "r", encoding='utf-16') as f:
        raw_output = f.read()
except FileNotFoundError:
    print("Errore: File 'calendar_output.txt' non trovato.")
    exit()

# Dizionari per raggruppare i dati
gironi = defaultdict(list) # Chiave: nome gruppo (es. 'g1'), Valore: lista squadre
giornate = defaultdict(list) # Chiave: numero giornata (int), Valore: lista di tuple (gruppo, squadra1, squadra2)

# --- Parsing dell'output di Clingo ---

# Cerca la linea che contiene gli atomi del modello (di solito dopo "Answer: X")
model_line = ""
lines = raw_output.splitlines()
for i, line in enumerate(lines):
    if line.strip().startswith("Answer:"):
        if i + 1 < len(lines):
            model_line = lines[i+1].strip() # La linea successiva contiene gli atomi
            break
    # Gestisce il caso in cui gli atomi siano sulla stessa riga di Answer: 1
    elif 'match_day' in line or 'team_in_group' in line and not line.strip().startswith("Reading from"):
         model_line = line.strip()
         # A volte Answer: 1 è sulla stessa linea degli atomi senza spazio
         model_line = model_line.replace("Answer: 1", "").strip()
         break


if not model_line:
    print("Non sono riuscito a trovare la linea con gli atomi del modello nell'output.")
    # Prova a cercare in tutto l'output come fallback (meno preciso)
    model_line = ' '.join(raw_output.split()) # Unisci tutto in una riga
    if 'match_day' not in model_line and 'team_in_group' not in model_line:
         print("Nessun atomo 'match_day' o 'team_in_group' trovato nell'output.")
         exit()

# Parsing dei team nei gironi usando la regex corretta
# team_in_group(g8,france)
for match in re.finditer(r'team_in_group\((\w+),(\w+)\)', model_line):
    # gruppo (es. "g1"), squadra (es. "france")
    girone_nome, squadra = match.groups() 
    gironi[girone_nome].append(squadra)

# Parsing delle partite usando la regex corretta
# match_day(3,brazil,croatia,g1)
for match in re.finditer(r'match_day\((\d+),(\w+),(\w+),(\w+)\)', model_line):
    # giornata (es. "3"), squadra1, squadra2, gruppo (es. "g1")
    giornata_num_str, squadra1, squadra2, girone_nome = match.groups()
    giornata_num = int(giornata_num_str)
    giornate[giornata_num].append((girone_nome, squadra1, squadra2))

# --- Stampa gironi ---
print("\n====================")
print("      GIRONI")
print("====================")
# Ordina i gironi per nome (g1, g2, ...)
for nome_girone in sorted(gironi.keys()):
    print(f"\nGirone {nome_girone.upper()}:")
    # Ordina le squadre alfabeticamente all'interno del girone
    for team in sorted(gironi[nome_girone]):
        print(f" - {team}")

# --- Stampa giornate ---
print("\n\n====================")
print("     CALENDARIO")
print("====================")
# Ordina le giornate numericamente (1, 2, 3)
for num_giornata in sorted(giornate.keys()):
    print(f"\nGiornata {num_giornata}:")
    # Ordina le partite di quella giornata (es. per gruppo, poi per squadra1)
    partite_ordinate = sorted(giornate[num_giornata], key=lambda x: (x[0], x[1]))
    for nome_girone, team1, team2 in partite_ordinate:
        # Stampa con il nome del gruppo corretto
        print(f" [{nome_girone.upper()}] {team1} vs {team2}")

print("\n") # Aggiunge una riga vuota alla fine per pulizia