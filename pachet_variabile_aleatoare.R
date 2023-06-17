
# ========================================================== Cerinta 2 ==========================================================

#   2. Fiind dată o funcție f , introdusă de utilizator, determinarea unei constante de
# normalizare k(cu precizarea dacă această constantă duce la o funcție de masă sau la o
# densitate de probabilitate). Ȋn cazul ȋn care o asemenea constantă nu există, afișarea
# unui mesaj corespunzător către utilizator.

# functia de masa -> pot stoca in lista
# =============================================================== Cum ar putea sa duca la o functie de masa sau densitate?
# =============================================================== E ok daca iau min/max sau ar trebui de la -inf la inf?

#     Constanta de normalizare este o constanta care, atunci cand este inmultita cu rezultatul integrarii
# functiei alese, intoarce valoare 1. Este utila aceasta constanta deoarece in statistica suma tuturor
# probabilitatilor trebuie sa fie egala cu 1. Intrucat aria de sub un grafic reprezinta tot spectrul 
# probabilistic al unei variabile aleatoare continue, integrala functiei graficului trebuie sa fie egala cu 1,
# aceasta functie fiind functia de densitate 

f <- function(x){
    # functie care intoarce 1 pt valori intre 0 si 1, si a^2 pentru valori mai mari decat 1 
    x^2
}

inf_f <- function(x){
    # functie care, integrata, rezulta infinit
    1 / (x - 1)
}

const * integrate(f, 0, 2)$value

result <- tryCatch(
      integrate(inf_f, lower = 0, upper = 1),
      error = function(err) {
          # Custom response when the integration results in an error
          #message("Integration resulted in an error: ", err$message)
          # You can choose to return a specific value or handle the error in a different way
          cat("salut sunt mircea")
          return(NA)
      }
)
print(result)

integrate(f, 0, 2)

a <- c(1.4, 2.1, 4.83, 7)
a = 1/sum(a) * a
a = NA
typeof(a)

# ok cu min si max
# undeva cand am relatia cu utilizatorul sa fie clar ca o constanta duce la

integreaza_finit <- function(f, min, max){
    rez_integrarii <- tryCatch(
        integrate(f, min, max), 
        error = function(err){
            # daca integrala tinde la infinit, mesajul erorii este cel de mai jos
            if(err$message == "non-finite function value"){
                cat("Functia data tinde la infinit.\nNu se poate gasi o constanta de normalizare pentru ea.\n")
            }
            else{
                cat("O eroare neasteptata a avut loc la integrare:\n")
                cat(err$message)
            }
            return(NA)
        }
    )
}

#daca e functie discreta: 
#     - are intrari invalide si intoarce 1
#     - are intrari valide si intoarce 2
#daca e functie continua:
#     - intoarce 0
verifica_functie_discreta <- function(x, min, max){
    if(typeof(x) != "function" && typeof(x) != "builtin" && typeof(x) != "closure"){
        if(min != max){
            cat("Pentru functiile de masa nu se introduc limite.\n
                 Pentru a obtine rezultatele functiei pe un interval specific introduceti alta functie discreta ce contine acel interval.\n")
            return(1)
        }
        tryCatch(
            sum <- sum(x),
            error = function(err){
                return(NA)
            }
        )
        
        if(is.na(sum)){
            cat("Functia introdusa nu este valida")
            return(1);
        }
        return(2)
    }
    return(0)
}

# daca functia e continua obtin o densitate
# pot da un cat sa zic daca e densitate
# nu orice functie poate fi densitate: definita in R cu val in R, sa 
obtine_constanta_normalizare <- function(f, min = 0, max = 0){
    # Aceasta metoda de obtinere a constantei de normalizare poate ajunge la rezultatul corect, 
    # fie ca functia f are valori discrete (functie de masa) ( acestea fiind reprezentate de felul ifelse(x < 1, 1, 2)
    # sau valori continue (functie de densitate) (x^2
    # min si max reprezinta intervalul pe care functia ia valori diferite de 0
    
    # verific daca f e functie continua sau concreta
    status_functie <- verifica_functie_discreta(f, min, max);
    
    if(status_functie == 2){
        sum <- sum(f)
        cat("S-a creat cu succes o constanta de normalizare pentru o functie de masa\n")
        return(1/sum)
    }
    if(status_functie == 1){
        return(NA)
    }
    
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
        return(NA)
    }
    # integrala poate tinde la infinit ceea ce face ca functia integrate sa afiseze o eroare. 
    # ce imi doresc este sa prind acea eroare si sa o tratez, ceea ce fac cu tryCatch():
    rez_integrarii <- integreaza_finit(f, min, max)
    # ne intereseaza constanta care, atunci cand este inmultita cu integrala, sa intoarca 1: 
    # k * integrala = 1
    # k = 1/integrala
    # folosesc $value deoarece vreau valoarea stocata in rezultatul integrarii
    if(typeof(rez_integrarii) == "logical"){
        if(is.na(rez_integrarii) == TRUE)
        return(NA)
    }
    cat("S-a creat cu succes o constanta de normalizare pentru o functie de densitate\n")
    return(1 / rez_integrarii$value)
}

const = obtine_constanta_normalizare(f, 0, 3)
const * integrate(f, 0, 3)$value

a <- c(1, 2)
const1 = obtine_constanta_normalizare(a)
const1 * sum(a)

inf_const = obtine_constanta_normalizare(inf_f, 0, 1)

obtine_constanta_normalizare(f, 1, 2) * integrate(f, 1, 2)$value

# ========================================================== Cerinta 3 ==========================================================

#     3) Reprezentarea grafică a densității de probabilitate/funcției de masă și a funcției de
# repartiție pentru diferite valori ale parametrilor repartiției. Ȋn cazul ȋn care funcția de
# repartiție nu este dată ȋntr-o formă explicită(ex. repartiția normală) se acceptă
# reprezentarea grafică a unei aproximări a acesteia.

# =============================================================== Nu prea inteleg cerinta daca ar putea clarifica
# =============================================================== Pur si simplu reprezint grafic mai multe repartitii? 
# =============================================================== Sau o functie data de user??
# =============================================================== diferite valori date de user?

#am o repartitie:  vreau sa vad functia de masa, repartitia pt diferite valori ale parametriilor
#cum se modifica forma functiei de masa, repartitie in functie de diferiti parametrii
# functia de repartitie e o integrala care nu se poate calcula -> nu reprezint grafic fix integrala -> 

#trebuie sa ii arat niste grafice legate de repartitii
# masa si repartitie pt geometrica: 
  # sa pot apela functia mea astfel incat sa arate asta

#poate cer f de densitate pt o densitate construita aici cu k
  #sa apelez cumva f3 pt a printa masa si repartitia asta


#fucntie de masa:

n <- -2:4
mass_f <- dbinom(x=n, size=2, prob=1/4, log = FALSE)
plot(n, mass_f, type='h')
sum(mass_f)

n <- 0:200
functie_de_masa <- function(n){
  mass_f <- dbinom(n, 200, .5, log = FALSE)
  plot(n, mass_f, type='h')
  mass_f
}

f_masa <- functie_de_masa(n)
sum(f_masa)

# ========================================================== Cerinta 4 ==========================================================

#   4) Calculul mediei, dispersiei și a momentelor inițiale și centrate pȃnă la ordinul 4(dacă
# există). Atunci cȃnd unul dintre momente nu există, se va afișa un mesaj corespunzător
#`către utilizator.

# =============================================================== Cum ar putea sa nu existe
# exista repartitii care nu au medie -> nu vor avea si momente
# pot avea integrale sau sume -> daca integrala e divergenta sa zica nu exista etc..

x <- c(1, 2, 3, 10)

medie_dispersie_momente <- function(x, min = 0, max = 0){
    #daca f este o functie discreta 
    
      
        # media aritmetica
        media <- mean(x)
        # dispersia, sau varianta, se obtine adunand diferenta dintre fiecare valoare
        # si medie la patrat. Suma respectiva se imparte la numarul de valori - 1
        # (ajustarea lui Bessel, care are drept scop corectarea unui bias)
        dispersia <- var(x)
        
        # media este chiar primul moment 
        # acest prim moment totusi poate coincide cu multe alte dataseturi
        # un al doilea moment este suma patratelor lui x supra n (nr de valori din x)
        momentul_2 <- sum(x^2) / length(x)
        momentul_3 <- sum(x^3) / length(x)
        momentul_4 <- sum(x^4) / length(x)
        # cu cat varianta e mai mare cu atat momentele 2, 3, 4 vor fi mai mari
        
        # scopul momentelor centrate este sa elimine la fiecare pas momentul precedent 
        
        momentul_2_centrat <- sum((x - media)^2) / length(x)
        momentul_3_centrat <- sum((x - media)^3) / length(x)
        momentul_4_centrat <- sum((x - media)^4) / length(x)
      
        return(list(media = media, 
                    dispersia = dispersia, 
                    
                    momentul_1 = media,
                    momentul_2 = momentul_2,
                    momentul_3 = momentul_3,
                    momentul_4 = momentul_4,
                    
                    momentul_2_centrat = momentul_2_centrat,
                    momentul_3_centrat = momentul_3_centrat,
                    momentul_4_centrat = momentul_4_centrat
                    
                    ))
    }
    #daca nu e discreta nu intra pe if si daca este iese din functie la return 
    
    media <- integreaza_finit(x, min, max)
}

a <- medie_dispersie_momente(x)


