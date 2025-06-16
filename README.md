# tfg-inferencia_causal
En aquest repositori hi ha els douments referents al TFG d'Estadítica Aplicada de Jordi Lahuerta Reig titulat "Bases de la inferència causal amb metalerners (T, S i X-lerner) i la seva aplicació"

### Resum

Aquest treball explora les bases de la inferència causal aplicada a l'estimació d'efectes heterogenis del tractament mitjançant meta-learners (S-, T- i X-learner). Es presenten els fonaments teòrics de la inferència causal, les assumpcions necessàries i la mesura dels efectes mitjans i condicionals del tractament (ATE i CATE). També s’introdueixen les estratègies principals per a l’anàlisi dels efectes heterogenis (HTE), centrant-se finalment en els meta-learners esmentats. A nivell pràctic, s’aplica aquesta metodologia a dades d’un assaig clínic sobre embarassos amb risc de baix pes fetal (SGA), comparant dues intervencions (dieta mediterrània i reducció de l’estrès). L’anàlisi mostra que l’efecte dels tractaments varia segons el perfil clínic de la mare, fet que posa de manifest la rellevància dels efectes heterogenis i el potencial de la inferència causal amb tècniques de machine learning per avançar cap a una medicina més personalitzada. El treball destaca el bon rendiment de l’X-learner en contextos amb dades reals i equilibrades.  
**Paraules clau**: Inferència causal, meta-learners, efectes heterogenis del tractament (HTE), medicina personalitzada, aprenentatge automàtic



### Resumen

Este trabajo explora las bases de la inferencia causal aplicada a la estimación de efectos heterogéneos del tratamiento mediante meta-learners (S-, T- y X-learner). Se presentan los fundamentos teóricos de la inferencia causal, las suposiciones necesarias y la medición de los efectos medios y condicionales del tratamiento (ATE y CATE). También se introducen las principales estrategias para el análisis de los efectos heterogéneos (HTE), centrándose finalmente en los meta-learners mencionados. A nivel práctico, se aplica esta metodología a datos de un ensayo clínico sobre embarazos con riesgo de bajo peso fetal (SGA), comparando dos intervenciones (dieta mediterránea y reducción del estrés). El análisis muestra que el efecto de los tratamientos varía según el perfil clínico de la madre, lo que pone de manifiesto la relevancia de los efectos heterogéneos y el potencial de la inferencia causal con técnicas de aprendizaje automático para avanzar hacia una medicina más personalizada. El trabajo destaca el buen rendimiento del X-learner en contextos con datos reales y equilibrados.  
**Palabras clave**: Inferencia causal, meta-learners, efectos heterogéneos del tratamiento (HTE), medicina personalizada, aprendizaje automático



### Abstract

This work explores the foundations of causal inference applied to the estimation of heterogeneous treatment effects using meta-learners (S-, T-, and X-learner). It presents the theoretical basis of causal inference, the required assumptions, and the measurement of average and conditional treatment effects (ATE and CATE). The main strategies for analyzing heterogeneous treatment effects (HTE) are introduced, with a specific focus on the mentioned meta-learners. On a practical level, this methodology is applied to data from a clinical trial involving pregnancies at risk of Small for Gestational Age (SGA), comparing two interventions (Mediterranean diet and stress reduction) against standard care. The analysis shows that treatment effects vary depending on the mother’s clinical profile, highlighting the relevance of heterogeneity and the potential of causal inference combined with machine learning techniques to support a more personalized approach to medicine. The study emphasizes the strong performance of the X-learner in real, balanced data contexts.  
**Keywords**: Causal inference, meta-learners, heterogeneous treatment effects (HTE), personalized medicine, machine learning


---

### Sobre els documents
Cal carregar el document que entrena els meta-learners i crea l'objecte "resultats" que guardara els ITE de totes les pacients amb tots els outcomes i tractaments. Serà necessari per despres poder fer les anàlisis del altre document.
És a dir, primer cal fer correr "entrenar el metalearners.R" per entrenar els meta-learners S, T i X. També seveix per estimar els ITE que s'utilitzaran per analitzar els resultats i a "analisi amb resultats.R".

També s'hi ha el codi, LaTex que s'ha utilitzat per generar l'informe final.
