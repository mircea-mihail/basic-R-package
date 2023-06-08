
# ========================================================== Cerinta 2 ==========================================================

#   2. Fiind dată o funcție f , introdusă de utilizator, determinarea unei constante de
# normalizare k(cu precizarea dacă această constantă duce la o funcție de masă sau la o
# densitate de probabilitate). Ȋn cazul ȋn care o asemenea constantă nu există, afișarea
# unui mesaj corespunzător către utilizator.


#     Constanta de normalizare este o constanta care, atunci cand este inmultita cu rezultatul integrarii
# functiei alese, intoarce valoare 1. Este utila aceasta constanta deoarece in statistica suma tuturor
# probabilitatilor trebuie sa fie egala cu 1. Intrucat aria de sub un grafic reprezinta tot spectrul 
# probabilistic al unei variabile aleatoare continue, integrala functiei graficului trebuie sa fie egala cu 1,
# aceasta functie fiind functia de densitate 

f <- function(x){
    # functie care intoarce 1 pt valori intre 0 si 1, si a^2 pentru valori mai mari decat 1 
    ifelse(x < 1, 1, 2)
}

inf_f <- function(x){
    # functie care, integrata, rezulta infinit
    1 / (x - 1)
}

result <- tryCatch(
    integrate(inf_f, lower = 0, upper = 1),
    error = function(err) {
    # Custom response when the integration results in an error
      message("Integration resulted in an error: ", err$message)
      # You can choose to return a specific value or handle the error in a different way
      return(NA)
    }
)
print(result)

integrate(f, 0, 2)

obtine_constanta_normalizare <- function(f, min = 0, max = 0){
    # Aceasta metoda de obtinere a constantei de normalizare poate ajunge la rezultatul corect, 
    # fie ca functia f are valori discrete (functie de masa) ( acestea fiind reprezentate de felul ifelse(x < 1, 1, 2)
    # sau valori continue (functie de densitate) (x^2
    # min si max reprezinta intervalul pe care functia ia valori diferite de 0
    if(min == max){
        if(min == 0){
            cat("Nu ati introdus limite!\n")
            cat("Introduceti un interval pe care functia ia valori.\n")
            return(NA)
        }
        #daca intervalul este 0, orice constanta inmultita cu 0 va da mereu 0
        cat("Limitele au aceeasi valoare!\n")
        cat("Introduceti un interval valid, in care limitele pe care functia ia valori au sens.\n")
        return(NA)
    }
    if(min > max){
        cat("Limita inferioara este mai mare decat cea superioara!\n")
        cat("Introduceti un interval valid, in care limitele pe care functia ia valori au sens.\n")
        return(NA);
    }
    # integrala poate tinde la infinit ceea ce face ca functia integrate sa afiseze o eroare. 
    # ce imi doresc este sa prind acea eroare si sa o tratez, ceea ce fac cu tryCatch():
    rez_integrarii <- tryCatch(
                          integrate(f, min, max), 
                          error = function(err){
                              # daca integrala tinde la infinit, mesajul erorii este cel de mai jos
                              if(err$message == "non-finite function value"){
                                  cat("Functia data tinde la infinit.\nNu se poate gasi o constanta de normalizare pentru ea.\n")
                              }
                              else{
                                  cat("O eroare neasteptata a avut loc la integrare:\n")
                                  cat(err$message);
                              }
                              return(NA)
                          })
    # ne intereseaza constanta care, atunci cand este inmultita cu integrala, sa intoarca 1: 
    # k * integrala = 1
    # k = 1/integrala
    # folosesc $value deoarece vreau valoarea stocata in rezultatul integrarii
    return(1 / rez_integrarii$value)
}

obtine_constanta_normalizare(f, 0, 3)

obtine_constanta_normalizare(f, 1, 2) * integrate(f, 1, 2)$value

# ========================================================== Cerinta 3 ==========================================================

#     3) Reprezentarea grafică a densității de probabilitate/funcției de masă și a funcției de
# repartiție pentru diferite valori ale parametrilor repartiției. Ȋn cazul ȋn care funcția de
# repartiție nu este dată ȋntr-o formă explicită(ex. repartiția normală) se acceptă
# reprezentarea grafică a unei aproximări a acesteia.


#fucntie de masa:

n <- -10:20
mass_f <- dbinom(x=n, size=10, prob=1, log = FALSE)
plot(n, mass_f, type='h')


n <- 0:200
functie_de_masa <- function(n){
  mass_f <- dbinom(n, 200, .3, log = FALSE)
  plot(n, mass_f, type='h')
  mass_f
}

f_masa <- functie_de_masa(n)
sum(f_masa)



