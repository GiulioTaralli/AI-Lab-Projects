% Settimane: da 1 a 24
week(1..24).

% Giorni: lun-sab (nelle settimane full-time), ven-sab (altre settimane)
day(mon; tue; wed; thu; fri; sat).
day_num(mon, 1).  
day_num(tue, 2).  
day_num(wed, 3).  
day_num(thu, 4).  
day_num(fri, 5).  
day_num(sat, 6).

% Ore per giorno: 8 ore lun-ven, 6 ore sab
hour(1..8).
hour_saturday(1..6).

% Full-time weeks: settimana 7 e 16
fulltime_week(7;16).

% Insegnamenti: nome, docente, ore totali
course("project_management", "muzzetto", 14).
course("fondamenti_di_ict_e_paradigmi_di_programmazione", "pozzato", 14).
course("linguaggi_di_markup", "schifanella_rossano", 20).
course("la_gestione_della_qualita", "tomatis", 10).
course("ambienti_di_sviluppo_e_linguaggi_client_side_per_il_web", "micalizio", 20).
course("progettazione_grafica_e_design_di_interfacce", "terranova", 10).
course("progettazione_di_basi_di_dati", "mazzei", 20).
course("strumenti_e_metodi_di_interazione_nei_social_media", "giordani", 14).
course("acquisizione_ed_elaborazione_di_immagini_statiche_grafica", "zanchetta", 14).
course("accessibilita_usabilita_nella_progettazione_multimediale", "gena", 14).
course("marketing_digitale", "muzzetto", 10).
course("elementi_di_fotografia_digitale", "vargiu", 10).
course("risorse_digitali_per_il_progetto_collaborazione_e_documentazione", "boniolo", 10).
course("tecnologie_server_side_per_il_web", "damiano", 20).
course("tecniche_e_strumenti_di_marketing_digitale", "zanchetta", 10).
course("introduzione_al_social_media_management", "suppini", 14).
course("acquisizione_ed_elaborazione_del_suono", "valle", 10).
course("acquisizione_ed_elaborazione_di_sequenze_di_immagini_digitali", "ghidelli", 20).
course("comunicazione_pubblicitaria_e_comunicazione_pubblica", "gabardi", 14).
course("semiologia_e_multimedialita", "santangelo", 10).
course("crossmedia_articolazione_delle_scritture_multimediali", "taddeo", 20).
course("grafica_3d", "gribaudo", 20).
course("progettazione_e_sviluppo_di_applicazioni_web_su_dispositivi_mobile_i", "schifanella_rossano", 10).
course("progettazione_e_sviluppo_di_applicazioni_web_su_dispositivi_mobile_ii", "schifanella_claudio", 10).
course("la_gestione_delle_risorse_umane", "lombardo", 10).
course("i_vincoli_giuridici_del_progetto_diritto_dei_media", "travostino", 10).

professor(P) :- course(_, P, _).

% —— fatti di propedeuticità ——
prereq("fondamenti_di_ict_e_paradigmi_di_programmazione", "ambienti_di_sviluppo_e_linguaggi_client_side_per_il_web").
prereq("ambienti_di_sviluppo_e_linguaggi_client_side_per_il_web", "progettazione_e_sviluppo_di_applicazioni_web_su_dispositivi_mobile_i").
prereq("progettazione_e_sviluppo_di_applicazioni_web_su_dispositivi_mobile_i", "progettazione_e_sviluppo_di_applicazioni_web_su_dispositivi_mobile_ii").
prereq("progettazione_di_basi_di_dati", "tecnologie_server_side_per_il_web").
prereq("linguaggi_di_markup", "ambienti_di_sviluppo_e_linguaggi_client_side_per_il_web").
prereq("project_management", "marketing_digitale").
prereq("marketing_digitale", "tecniche_e_strumenti_di_marketing_digitale").
prereq("project_management", "strumenti_e_metodi_di_interazione_nei_social_media").
prereq("project_management", "progettazione_grafica_e_design_di_interfacce").
prereq("acquisizione_ed_elaborazione_di_immagini_statiche_grafica", "elementi_di_fotografia_digitale").
prereq("elementi_di_fotografia_digitale", "acquisizione_ed_elaborazione_di_sequenze_di_immagini_digitali").
prereq("acquisizione_ed_elaborazione_di_immagini_statiche_grafica", "grafica_3d").