# Youth-Impact-RA-Task
Hiring task for Youth Impact RA Role
R&I Research Associate Hiring
Data cleaning and analysis deliverable 


Context: Youth Impact's mEducation program delivers phone-based tutoring to primary school students in the Philippines. Teachers should first run a sensitization call (baseline assessment), then deliver 6 weekly tutoring calls, then an endline call (final assessment). Each phone call attempt is supposed to be logged as one SurveyCTO submission. 
Materials provided: SurveyCTO form definitions for all phases and de-identified raw datasets of survey submissions for sensitization, implementation, and endline.


Task A: Clean and merge the data
Produce a single dataset with one row per student, containing their sensitization result, their result for each implementation week, and their endline result. Each raw dataset may contain multiple submissions per student — you'll need to decide how to reduce these to one record per student per phase, and be ready to explain your reasoning (see Task C). 
⇒ Deliverable: STATA Do-File(s) or R Scripts


Task B: KPI graphs
Use your cleaned dataset to produce a small number of graphs on students' learning outcomes and the program's implementation fidelity (e.g. change in numeracy level, number of tutoring calls completed). We want you to show our three KPIs (% students mastering division, % students not mastering any operation, % students learning a new operation from base- to endline) as well as any other learning or implementation fidelity graph you find important.
⇒ Deliverable: STATA Do-File(s) or R Scripts


Task C: Slide deck
Prepare a short slide deck (~6–8 slides) with two parts: 
(1) Findings — your key KPI graphs and 2–3 takeaways on what the data tell us about the program, including any issues you notice with the program or the data; 
(2) Process — briefly walk through how you cleaned and merged the data, the judgment calls you made (e.g. handling duplicate submissions or missing call attempts), and any data quality issues you ran into and how you resolved them. Include suggested changes to our data collection system that would reduce / avoid the occurrence of these data quality issues.
⇒ Deliverable: Slide deck


On AI tools: You're welcome to use AI tools at any stage but please share your AI conversation with us. We're evaluating your judgment and understanding of the data, not just whether the code runs, so please be prepared to walk us through any part of your approach in a follow-up conversation.
