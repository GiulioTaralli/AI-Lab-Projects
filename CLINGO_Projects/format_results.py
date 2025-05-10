import re
from collections import defaultdict

def format_results_calendar(raw_output):
    # Dizionari per raggruppare i dati
    gironi = defaultdict(list) # Chiave: nome gruppo (es. 'g1'), Valore: lista squadre
    giornate = defaultdict(list) # Chiave: numero giornata (int), Valore: lista di tuple (gruppo, squadra1, squadra2)
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

    # ==============================
    #     TEMPO DI ESECUZIONE
    # ==============================
    for line in lines:
        if line.strip().startswith("Time"):
            print("\n===============================")
            print("     TEMPO DI ESECUZIONE")
            print("===============================")
            print(line.strip())
            print("\n")
            break

def format_results_master(raw_output):
    # Parsing dei fatti dal modello Clingo
    model_line = ""
    lines = raw_output.splitlines()
    for i, line in enumerate(lines):
        if line.strip().startswith("Answer:"):
            if i + 1 < len(lines):
                model_line = lines[i + 1].strip()
                break
        elif any(k in line for k in ("presentation", "lesson", "free_block", "first_week", "last_week")) \
             and not line.strip().startswith("Reading from"):
            model_line = line.strip().replace("Answer: 1", "").strip()
            break

    if not model_line:
        print("Non sono riuscito a trovare la linea con gli atomi del modello.")
        return

    # Ordine dei giorni
    giorno_ordine = {'mon': 1, 'tue': 2, 'wed': 3, 'thu': 4, 'fri': 5, 'sat': 6, 'sun': 7}

    # Raccoglie lezioni/presentazioni/free_block
    eventi = []

    for match in re.finditer(r'(presentation|lesson_with_teacher|free_block)\(([^)]+)\)', model_line):
        tipo, args = match.groups()
        args_split = [a.strip('"') for a in args.split(',')]
        if tipo == 'lesson_with_teacher':
            corso, week, day, slot, teacher = args_split
        else:
            week, day, slot = args_split
            corso = None
            teacher = None
        eventi.append((int(week), giorno_ordine[day], int(slot), tipo, day, corso, teacher))

    # Stampa ordinata degli eventi
    eventi.sort()
    current_week = None
    current_day = None

    print("\n======================")
    print("     CALENDARIO")
    print("======================")

    free_block_count = 0
    for week, day_num, slot, tipo, day_str, corso, teacher in eventi:
        if week != current_week:
            print(f"\nSettimana {week}")
            current_week = week
            current_day = None

        if day_str != current_day:
            print(f"\n  {day_str.capitalize()}")
            current_day = day_str

        if tipo == 'lesson_with_teacher':
            print(f"    Slot {slot}: {corso} - Prof. {teacher}")
        elif tipo == 'presentation':
            print(f"    Slot {slot}: Presentazione")
        elif tipo == 'free_block':
            print(f"    Slot {slot}: Blocco libero")
            print(f"    Slot {slot + 1}: Blocco libero")
            free_block_count += 1

    # Parsing first_week, last_week e span_info
    first_week_info = {}
    last_week_info = {}
    span_info = {}

    for match in re.finditer(r'first_week\("([^"]+)",(\d+),(\w+),(\d+)\)', model_line):
        corso, settimana, giorno, slot = match.groups()
        first_week_info[corso] = (int(settimana), giorno, int(slot))

    for match in re.finditer(r'last_week\("([^"]+)",(\d+),(\w+),(\d+)\)', model_line):
        corso, settimana, giorno, slot = match.groups()
        last_week_info[corso] = (int(settimana), giorno, int(slot))

    for match in re.finditer(r'span_info\("([^"]+)",(\d+),(\d+),(\d+)\)', model_line):
        corso, start_week, end_week, durata = match.groups()
        span_info[corso] = int(durata) - 1

    print("\n===============================")
    print("     INIZIO E FINE CORSI")
    print("===============================")

    # Ordina per data di inizio: settimana, giorno, slot
    giorno_ordine = {'mon': 1, 'tue': 2, 'wed': 3, 'thu': 4, 'fri': 5, 'sat': 6, 'sun': 7}
    corsi_ordinati = sorted(first_week_info.items(), key=lambda x: (
        x[1][0],                    # settimana
        giorno_ordine.get(x[1][1], 99),  # giorno
        x[1][2]                     # slot
    ))

    for corso, (start_sett, start_giorno, start_slot) in corsi_ordinati:
        end_sett, end_giorno, end_slot = last_week_info.get(corso, ("?", "?", "?"))
        durata = span_info.get(corso, "?")
        print(f"{corso}")
        print(f"   Start: settimana {start_sett}, {start_giorno}, ora {start_slot}")
        print(f"   End:   settimana {end_sett}, {end_giorno}, ora {end_slot}")
        print(f"   Time span: {durata} settimane\n")
    
    print(f"Numero blocchi liberi: {free_block_count}")

    # ==============================
    #     TEMPO DI ESECUZIONE
    # ==============================
    for line in lines:
        if line.strip().startswith("Time"):
            print("\n===============================")
            print("     TEMPO DI ESECUZIONE")
            print("===============================")
            print(line.strip())
            print("\n")
            break

def read_output_file(path):
    try:
        with open(path, "r", encoding='utf-16') as f:
            return f.read()
    except FileNotFoundError:
        print(f"Errore: File '{path}' non trovato.")
        exit()

def detect_source_file(raw_output):
    for line in raw_output.splitlines():
        if line.strip().startswith("Reading from"):
            if "calendar_generator.lp" in line:
                return "calendar_generator"
            elif "master_program_scheduler.lp" in line:
                return "master_program_scheduler"
    return None

def dispatch_formatter(source_file, raw_output):
    if source_file == "calendar_generator":
        format_results_calendar(raw_output)
    elif source_file == "master_program_scheduler":
        format_results_master(raw_output)

# Entry point
if __name__ == "__main__":
    raw_output = read_output_file("output.txt")
    source_file = detect_source_file(raw_output)
    if not source_file:
        print("Impossibile determinare il tipo di output Clingo.")
    else:
        dispatch_formatter(source_file, raw_output)