
# ========================================================== Cerinta 2 ==========================================================

#   2. Fiind dată o funcție f , introdusă de utilizator, determinarea unei constante de
# normalizare k(cu precizarea dacă această constantă duce la o funcție de masă sau la o
# densitate de probabilitate). Ȋn cazul ȋn care o asemenea constantă nu există, afișarea
# unui mesaj corespunzător către utilizator.


#incearca sa integreze funcita si intoarce NA daca nu se poate sau rezultatul integrarii daca da
integreaza_finit <- function(f, min, max){
    rez_integrarii <- tryCatch(
        integrate(f, min, max), 
        error = function(err){
            # daca integrala tinde la infinit, mesajul erorii este cel de mai jos
            if(err$message == "non-finite function value"){
                cat("Functia data tinde la infinit.\n")
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
            cat("Functia introdusa nu este valida\n")
            return(1);
        }
        if(typeof(x) == "logical"){
          cat("Tipul functiei introduse este invalid: logical\n")
          return(1)
        }
        return(2)
    }
    return(0)
}

# daca functia e continua obtin o densitate
# pot da un cat sa zic daca e densitate
# nu orice functie poate fi densitate: definita in R cu val in R, sa 
# constanta de normalizare se realizeaza cu ajutorul probabilitatilor fiecarei functie -> suma lor sa fie 1
obtine_constanta_normalizare <- function(f, min = 0, max = 0){
    # Aceasta metoda de obtinere a constantei de normalizare poate ajunge la rezultatul corect, 
    # fie ca functia f are valori discrete (functie de masa) ( acestea fiind reprezentate de felul ifelse(x < 1, 1, 2)
    # sau valori continue (functie de densitate) (x^2
    # min si max reprezinta intervalul pe care functia ia valori diferite de 0
    
    # verific daca f e functie continua sau concreta
    status_functie <- verifica_functie_discreta(f, min, max)
    
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


# ========================================================== Cerinta 3 ==========================================================

#     3) Reprezentarea grafică a densității de probabilitate/funcției de masă și a funcției de
# repartiție pentru diferite valori ale parametrilor repartiției. Ȋn cazul ȋn care funcția de
# repartiție nu este dată ȋntr-o formă explicită(ex. repartiția normală) se acceptă
# reprezentarea grafică a unei aproximări a acesteia.

#daca nu se dau alte inputuri se fac 4 plotari generice cu un parametru schimbator
#daca se dau inputuri pt fiecare parametru se fac doar 2 plotari pt cele 2 functii cerute(Dens si rep)
reprezentare_grafica <- function(index_f=0, scale_f = 100, param_1 = -1, param_2 = -1){
    if(index_f == 0){
        cat("distributii implementate:\n1 - distributia binomiala\n2 - distributia normala\n3 - distributia geometrica\n")
        cat("4 - distributia exponentiala\n5 - distributia uniforma\n6 - distributia poisson")
        return()
    }
    #repartitia binomiala -> pot fi modificate numarul de incercari si probabilitatea de success
    if(index_f == 1){
        if(param_1 == -1 || param_2 == -1){
            par(mfrow=c(2,2))
          
            cat("Pentru o afisare mai concreta introduceti valori pentru ambii parametrii cat si pentru scala")
            plot(0:scale_f, dbinom(0:scale_f, 0.1*scale_f, 0.5), col="#1C4700", type="l", lwd="3", 
                ylab="probabilitatea", xlab="numarul de incercari", main="Densitatea binomialei cu numarul de incercari variabil")
            lines(0:scale_f, dbinom(0:scale_f, 0.4*scale_f, 0.5), col="#3F9F00", type="l", lwd="3")
            lines(0:scale_f, dbinom(0:scale_f, 0.7*scale_f, 0.5), col="#5be600", type="l", lwd="3")
            lines(0:scale_f, dbinom(0:scale_f, 1*scale_f, 0.5), col="#89FF3C", type="l", lwd="3")
            lines(0:scale_f, dbinom(0:scale_f, 1.3*scale_f, 0.5), col="#C0FF98", type="l", lwd="3")
            
            plot(0:scale_f, dbinom(0:scale_f, scale_f, 0.1), col="#d698ff", type="l", lwd="3",
                 ylab="probabilitatea", xlab="numarul de incercari", main="Densitatea binomialei cu probabilitatea de success variabila")
            lines(0:scale_f, dbinom(0:scale_f, scale_f, 0.3), col="#b23cff", type="l", lwd="3")
            lines(0:scale_f, dbinom(0:scale_f, scale_f, 0.5), col="#8b00e6", type="l", lwd="3")
            lines(0:scale_f, dbinom(0:scale_f, scale_f, 0.7), col="#60009f", type="l", lwd="3")
            lines(0:scale_f, dbinom(0:scale_f, scale_f, 0.9), col="#2b0047", type="l", lwd="3")
            
            plot(0:scale_f, pbinom(0:scale_f, 0.1*scale_f, 0.5), col="#1C4700", type="l", lwd="3",
                 ylab="probabilitatea", xlab="numarul de incercari", main="Repartitia binomialei cu numarul de incercari variabil")
            lines(0:scale_f, pbinom(0:scale_f, 0.4*scale_f, 0.5), col="#3F9F00", type="l", lwd="3")
            lines(0:scale_f, pbinom(0:scale_f, 0.7*scale_f, 0.5), col="#5be600", type="l", lwd="3")
            lines(0:scale_f, pbinom(0:scale_f, 1*scale_f, 0.5), col="#89FF3C", type="l", lwd="3")
            lines(0:scale_f, pbinom(0:scale_f, 1.3*scale_f, 0.5), col="#C0FF98", type="l", lwd="3")
            
            plot(0:scale_f, pbinom(0:scale_f, scale_f, 0.1), col="#d698ff", type="l", lwd="3",
                 ylab="probabilitatea", xlab="numarul de incercari", main="Repartitia binomialei cu probabilitatea de success variabila")
            lines(0:scale_f, pbinom(0:scale_f, scale_f, 0.3), col="#b23cff", type="l", lwd="3")
            lines(0:scale_f, pbinom(0:scale_f, scale_f, 0.5), col="#8b00e6", type="l", lwd="3")
            lines(0:scale_f, pbinom(0:scale_f, scale_f, 0.7), col="#60009f", type="l", lwd="3")
            lines(0:scale_f, pbinom(0:scale_f, scale_f, 0.9), col="#2b0047", type="l", lwd="3")
            return()
        }
        par(mfrow=c(1,2))
      
        plot(0:scale_f, dbinom(0:scale_f, param_1, param_2), col="#5be600", type="l", lwd="3", 
            ylab="probabilitatea", xlab="numarul de incercari", main="Densitatea binomialei cu valorile alese")
        plot(0:scale_f, pbinom(0:scale_f, scale_f, 0.1), col="#8b00e6", type="l", lwd="3",
             ylab="probabilitatea", xlab="numarul de incercari", main="Repartitia binomialei cu valorile alese")
        
        return()
    }

    #repartitia normala -> pot fi modificate media si deviatia standard
    if(index_f == 2){
        if(param_1 == -1 || param_2 == -1){
            par(mfrow=c(2,2))
            
            cat("Pentru o afisare mai concreta introduceti valori pentru ambii parametrii cat si pentru scala")
            plot(0:scale_f, dnorm(0:scale_f, 0.3*scale_f, 0.1*scale_f), col="#1C4700", type="l", lwd="3", 
                 ylab="probabilitatea", xlab="valoarea variabilei aleatoare", main="Densitatea normalei cu media variabila")
            lines(0:scale_f, dnorm(0:scale_f, 0.4*scale_f, 0.1*scale_f), col="#3F9F00", type="l", lwd="3")
            lines(0:scale_f, dnorm(0:scale_f, 0.5*scale_f, 0.1*scale_f), col="#5be600", type="l", lwd="3")
            lines(0:scale_f, dnorm(0:scale_f, 0.6*scale_f, 0.1*scale_f), col="#89FF3C", type="l", lwd="3")
            lines(0:scale_f, dnorm(0:scale_f, 0.7*scale_f, 0.1*scale_f), col="#C0FF98", type="l", lwd="3")
            
            plot(0:scale_f, dnorm(0:scale_f, 0.5*scale_f, 0.02*scale_f), col="#d698ff", type="l", lwd="3",
                 ylab="probabilitatea", xlab="valoarea variabilei aleatoare", main="Densitatea normalei cu deviatia standard variabila")
            lines(0:scale_f, dnorm(0:scale_f, 0.5*scale_f, 0.03*scale_f), col="#b23cff", type="l", lwd="3")
            lines(0:scale_f, dnorm(0:scale_f, 0.5*scale_f, 0.04*scale_f), col="#8b00e6", type="l", lwd="3")
            lines(0:scale_f, dnorm(0:scale_f, 0.5*scale_f, 0.07*scale_f), col="#60009f", type="l", lwd="3")
            lines(0:scale_f, dnorm(0:scale_f, 0.5*scale_f, 0.13*scale_f), col="#2b0047", type="l", lwd="3")
            
            plot(0:scale_f, pnorm(0:scale_f, 0.2*scale_f, 0.1*scale_f), col="#1C4700", type="l", lwd="3",
                 ylab="probabilitatea", xlab="valoarea variabilei aleatoare", main="Repartitia normalei cu media variabila")
            lines(0:scale_f, pnorm(0:scale_f, 0.35*scale_f, 0.1*scale_f), col="#3F9F00", type="l", lwd="3")
            lines(0:scale_f, pnorm(0:scale_f, 0.5*scale_f, 0.1*scale_f), col="#5be600", type="l", lwd="3")
            lines(0:scale_f, pnorm(0:scale_f, 0.65*scale_f, 0.1*scale_f), col="#89FF3C", type="l", lwd="3")
            lines(0:scale_f, pnorm(0:scale_f, 0.8*scale_f, 0.1*scale_f), col="#C0FF98", type="l", lwd="3")
            
            plot(0:scale_f, pnorm(0:scale_f, 0.5*scale_f, 0.02*scale_f), col="#d698ff", type="l", lwd="3",
                 ylab="probabilitatea", xlab="valoarea variabilei aleatoare", main="Repartitia normalei cu deviatia standard variabila")
            lines(0:scale_f, pnorm(0:scale_f, 0.5*scale_f, 0.05*scale_f), col="#b23cff", type="l", lwd="3")
            lines(0:scale_f, pnorm(0:scale_f, 0.5*scale_f, 0.1*scale_f), col="#8b00e6", type="l", lwd="3")
            lines(0:scale_f, pnorm(0:scale_f, 0.5*scale_f, 0.2*scale_f), col="#60009f", type="l", lwd="3")
            lines(0:scale_f, pnorm(0:scale_f, 0.5*scale_f, 0.4*scale_f), col="#2b0047", type="l", lwd="3")
            return()
        }
        par(mfrow=c(1,2))
      
        plot(0:scale_f, dnorm(0:scale_f, param_1, param_2), col="#5be600", type="l", lwd="3", 
             ylab="probabilitatea", xlab="valoarea variabilei aleatoare", main="Densitatea normalei cu valorile alese")
        plot(0:scale_f, pnorm(0:scale_f, param_1, param_2), col="#8b00e6", type="l", lwd="3",
             ylab="probabilitatea", xlab="valoarea variabilei aleatoare", main="Repartitia normalei cu valorile alese")
        return()
    }
    
    #repartitia geometrica
    if(index_f == 3){
        if(param_1 == -1){
            par(mfrow=c(1,2))
            
            cat("Pentru o afisare mai concreta introduceti valori pentru ambii parametrii cat si pentru scala")
            plot(0:scale_f, dgeom(0:scale_f, 0.09), col="#1C4700", type="l", lwd="3", 
                 ylab="probabilitatea", xlab="numarul de incercari", main="Densitatea geometricei cu probabilitatea de success variabila")
            lines(0:scale_f, dgeom(0:scale_f, 0.2), col="#3F9F00", type="l", lwd="3")
            lines(0:scale_f, dgeom(0:scale_f, 0.35), col="#5be600", type="l", lwd="3")
            lines(0:scale_f, dgeom(0:scale_f, 0.5), col="#89FF3C", type="l", lwd="3")
            lines(0:scale_f, dgeom(0:scale_f, 0.7), col="#C0FF98", type="l", lwd="3")
            
            plot(0:scale_f, pgeom(0:scale_f, 0.09), col="#d698ff", type="l", lwd="3",
                 ylab="probabilitatea", xlab="numarul de incercari", main="Repartitia geometricei cu probabilitatea de success variabila")
            lines(0:scale_f, pgeom(0:scale_f, 0.2), col="#b23cff", type="l", lwd="3")
            lines(0:scale_f, pgeom(0:scale_f, 0.35), col="#8b00e6", type="l", lwd="3")
            lines(0:scale_f, pgeom(0:scale_f, 0.5), col="#60009f", type="l", lwd="3")
            lines(0:scale_f, pgeom(0:scale_f, 0.7), col="#2b0047", type="l", lwd="3")
            return()
        }
        par(mfrow=c(1,2))
        
        plot(0:scale_f, dgeom(0:scale_f, param_1), col="#5be600", type="l", lwd="3", 
             ylab="probabilitatea", xlab="numarul de incercari", main="Densitatea geometricei cu valorile alese")
        plot(0:scale_f, pgeom(0:scale_f, param_1), col="#8b00e6", type="l", lwd="3",
             ylab="probabilitatea", xlab="numarul de incercari", main="Repartitia geometricei cu valorile alese")
        
        return()
    }
  
    #repartitia exponentiala
    if(index_f == 4){
        if(param_1 == -1){
            par(mfrow=c(1,2))
            
            cat("Pentru o afisare mai concreta introduceti valori pentru ambii parametrii cat si pentru scala")
            plot(0:scale_f, dexp(0:scale_f, 0.09), col="#1C4700", type="l", lwd="3", 
                 ylab="probabilitatea", xlab="numarul de incercari", main="Densitatea exponentialei cu probabilitatea de success variabila")
            lines(0:scale_f, dexp(0:scale_f, 0.2), col="#3F9F00", type="l", lwd="3")
            lines(0:scale_f, dexp(0:scale_f, 0.35), col="#5be600", type="l", lwd="3")
            lines(0:scale_f, dexp(0:scale_f, 0.5), col="#89FF3C", type="l", lwd="3")
            lines(0:scale_f, dexp(0:scale_f, 0.7), col="#C0FF98", type="l", lwd="3")
            
            plot(0:scale_f, pexp(0:scale_f, 0.09), col="#d698ff", type="l", lwd="3",
                 ylab="probabilitatea", xlab="numarul de incercari", main="Repartitia exponentialei cu probabilitatea de success variabila")
            lines(0:scale_f, pexp(0:scale_f, 0.2), col="#b23cff", type="l", lwd="3")
            lines(0:scale_f, pexp(0:scale_f, 0.35), col="#8b00e6", type="l", lwd="3")
            lines(0:scale_f, pexp(0:scale_f, 0.5), col="#60009f", type="l", lwd="3")
            lines(0:scale_f, pexp(0:scale_f, 0.7), col="#2b0047", type="l", lwd="3")
            return()
        }
        par(mfrow=c(1,2))
        
        plot(0:scale_f, dexp(0:scale_f, param_1), col="#5be600", type="l", lwd="3", 
             ylab="probabilitatea", xlab="numarul de incercari", main="Densitatea exponentialei cu valorile alese")
        plot(0:scale_f, pexp(0:scale_f, param_1), col="#8b00e6", type="l", lwd="3",
             ylab="probabilitatea", xlab="numarul de incercari", main="Repartitia exponentialei cu valorile alese")
        
        return()
    }
  
    #repartitia uniforma
    if(index_f == 5){
        if(param_1 == -1){
            par(mfrow=c(1,2))
            
            cat("Pentru o afisare mai concreta introduceti valori pentru ambii parametrii cat si pentru scala")
            plot(0:scale_f, dunif(0:scale_f, 0.1*scale_f, 0.2*scale_f), col="#1C4700", type="l", lwd="3", 
                 ylab="probabilitatea", xlab="numarul de incercari", main="Densitatea uniformei cu minimul si maximul schimbate")
            lines(0:scale_f, dunif(0:scale_f, 0*scale_f, 0.3*scale_f), col="#3F9F00", type="l", lwd="3")
            lines(0:scale_f, dunif(0:scale_f, 0.2*scale_f, 0.6*scale_f), col="#5be600", type="l", lwd="3")
            lines(0:scale_f, dunif(0:scale_f, 0.8*scale_f, 0.9*scale_f), col="#89FF3C", type="l", lwd="3")
            lines(0:scale_f, dunif(0:scale_f, 0*scale_f, 1*scale_f), col="#C0FF98", type="l", lwd="3")
            
            plot(0:scale_f, punif(0:scale_f, 0.1*scale_f, 0.2*scale_f), col="#d698ff", type="l", lwd="3", 
                 ylab="probabilitatea", xlab="numarul de incercari", main="Repartitia uniformei cu minimul si maximul schimbate")
            lines(0:scale_f, punif(0:scale_f, 0*scale_f, 0.3*scale_f), col="#b23cff", type="l", lwd="3")
            lines(0:scale_f, punif(0:scale_f, 0.2*scale_f, 0.6*scale_f), col="#8b00e6", type="l", lwd="3")
            lines(0:scale_f, punif(0:scale_f, 0.8*scale_f, 0.9*scale_f), col="#60009f", type="l", lwd="3")
            lines(0:scale_f, punif(0:scale_f, 0*scale_f, 1*scale_f), col="#2b0047", type="l", lwd="3")
            
            return()
        }
        par(mfrow=c(1,2))
        
        plot(0:scale_f, dunif(0:scale_f, param_1, param_2), col="#5be600", type="l", lwd="3", 
             ylab="probabilitatea", xlab="numarul de incercari", main="Densitatea uniformei cu valorile alese")
        plot(0:scale_f, punif(0:scale_f, param_1, param_2), col="#8b00e6", type="l", lwd="3",
             ylab="probabilitatea", xlab="numarul de incercari", main="Repartitia uniformei cu valorile alese")
        
        return()
    }
  
    #repartitia poisson
    if(index_f == 6){
        if(param_1 == -1){
            par(mfrow=c(1,2))
            
            cat("Pentru o afisare mai concreta introduceti valori pentru ambii parametrii cat si pentru scala")
            plot(0:scale_f, dpois(0:scale_f, 0.1*scale_f), col="#1C4700", type="l", lwd="3", 
             ylab="probabilitatea", xlab="numarul de evenimente", main="Densitatea poisson cu numarul de evenimente variabil")
            lines(0:scale_f, dpois(0:scale_f, 0.25*scale_f), col="#3F9F00", type="l", lwd="3")
            lines(0:scale_f, dpois(0:scale_f, 0.4*scale_f), col="#5be600", type="l", lwd="3")
            lines(0:scale_f, dpois(0:scale_f, 0.55*scale_f), col="#89FF3C", type="l", lwd="3")
            lines(0:scale_f, dpois(0:scale_f, 0.7*scale_f), col="#C0FF98", type="l", lwd="3")
            
            plot(0:scale_f, ppois(0:scale_f, 0.1*scale_f), col="#d698ff", type="l", lwd="3", 
               ylab="probabilitatea", xlab="numarul de evenimente", main="Repartitia poisson cu numarul de evenimente variabil")
            lines(0:scale_f, ppois(0:scale_f, 0.25*scale_f), col="#b23cff", type="l", lwd="3")
            lines(0:scale_f, ppois(0:scale_f, 0.4*scale_f), col="#8b00e6", type="l", lwd="3")
            lines(0:scale_f, ppois(0:scale_f, 0.55*scale_f), col="#60009f", type="l", lwd="3")
            lines(0:scale_f, ppois(0:scale_f, 0.7*scale_f), col="#2b0047", type="l", lwd="3")
            
            return()
        }
        par(mfrow=c(1,2))
        
        plot(0:scale_f, dpois(0:scale_f, param_1), col="#5be600", type="l", lwd="3", 
             ylab="probabilitatea", xlab="numarul de incercari", main="Densitatea poisson cu valorile alese")
        plot(0:scale_f, ppois(0:scale_f, param_1), col="#8b00e6", type="l", lwd="3",
             ylab="probabilitatea", xlab="numarul de incercari", main="Repartitia poisson cu valorile alese")
        
        return()
    }
}

# ========================================================== Cerinta 4 ==========================================================

#   4) Calculul mediei, dispersiei și a momentelor inițiale și centrate pȃnă la ordinul 4(dacă
# există). Atunci cȃnd unul dintre momente nu există, se va afișa un mesaj corespunzător
#`către utilizator.

# f pentru variabila aleatoare discreta este produsul probabilitate-valoare ca mai sus
medie_dispersie_momente <- function(f, min = 0, max = 0){
    if(typeof(f) == "list"){
        cat("Introduceti o functie continua sau un vector. Listele nu se accepta.\n")
        return(NA)
    }
    #daca f este o functie discreta 
    status_functie <- verifica_functie_discreta(f, min, max)
    if(status_functie == 1){
        return(NA)
    }
    
    if(status_functie == 2){
        # media aritmetica
        media <- mean(f)
        # dispersia, sau varianta, se obtine adunand diferenta dintre fiecare valoare
        # si medie la patrat. Suma respectiva se imparte la numarul de valori - 1
        # (ajustarea lui Bessel, care are drept scop corectarea unui bias)
        dispersia <- var(f)
        
        # media este chiar primul moment 
        # acest prim moment totusi poate coincide cu multe alte dataseturi
        # un al doilea moment este suma patratelor lui f supra n (nr de valori din f)
        momentul_2 <- sum(f^2) / length(f)
        momentul_3 <- sum(f^3) / length(f)
        momentul_4 <- sum(f^4) / length(f)
        # cu cat varianta e mai mare cu atat momentele 2, 3, 4 vor fi mai mari
        
        # scopul momentelor centrate este sa elimine la fiecare pas momentul precedent 
        
        momentul_2_c <- sum((f - media)^2) / length(f)
        momentul_3_c <- sum((f - media)^3) / length(f)
        momentul_4_c <- sum((f - media)^4) / length(f)
      
        return(list(media = media, 
                    dispersia = dispersia, 
                    
                    momentul_1 = media,
                    momentul_2 = momentul_2,
                    momentul_3 = momentul_3,
                    momentul_4 = momentul_4,
                    
                    momentul_2_centrat = momentul_2_c,
                    momentul_3_centrat = momentul_3_c,
                    momentul_4_centrat = momentul_4_c
                    
                    )
               )
    }
    #daca nu e discreta nu intra pe if si daca este iese din functie la return 
    if(min > max){
        cat("Limita inferioara este mai mare decat cea superioara!\n")
        cat("Introduceti un interval valid, in care limitele pe care functia ia valori au sens.\n")
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
    
    if(typeof(integreaza_finit(f, min, max)) == "logical"){
        return(NA)
    }
    
    m1_func <- function(x){
        x * f(x)
    }
    m2_func <- function(x){
        x^2 * f(x)
    }
    m3_func <- function(x){
        x^3 * f(x)
    }
    m4_func <- function(x){
        x^4 * f(x)
    }
    media      <- if(typeof(integreaza_finit(m1_func, min, max)) != "logical") integreaza_finit(m1_func, min, max)$value else NA
    momentul_1 <- if(typeof(integreaza_finit(m1_func, min, max)) != "logical") integreaza_finit(m1_func, min, max)$value else NA
    momentul_2 <- if(typeof(integreaza_finit(m2_func, min, max)) != "logical") integreaza_finit(m2_func, min, max)$value else NA
    momentul_3 <- if(typeof(integreaza_finit(m3_func, min, max)) != "logical") integreaza_finit(m3_func, min, max)$value else NA
    momentul_4 <- if(typeof(integreaza_finit(m4_func, min, max)) != "logical") integreaza_finit(m4_func, min, max)$value else NA
    
    m1_c_func <- function(x){
        (x-media) * f(x)
    }
    m2_c_func <- function(x){
        (x-media)^2 * f(x)
    }
    m3_c_func <- function(x){
        (x-media)^3 * f(x)
    }
    m4_c_func <- function(x){
        (x-media)^4 * f(x)
    }
    momentul_2_c <- if(typeof(integreaza_finit(m2_c_func, min, max)) != "logical") integreaza_finit(m2_c_func, min, max)$value else NA
    momentul_3_c <- if(typeof(integreaza_finit(m3_c_func, min, max)) != "logical") integreaza_finit(m3_c_func, min, max)$value else NA
    momentul_4_c <- if(typeof(integreaza_finit(m4_c_func, min, max)) != "logical") integreaza_finit(m4_c_func, min, max)$value else NA
    
    #varianta sau dispersia e chiar al doilea moment centrat
    dispersia <- momentul_2_c
    
    return(list(media = media, 
                dispersia = dispersia, 
                
                momentul_1 = momentul_1,
                momentul_2 = momentul_2,
                momentul_3 = momentul_3,
                momentul_4 = momentul_4,
                
                momentul_2_centrat = momentul_2_c,
                momentul_3_centrat = momentul_3_c,
                momentul_4_centrat = momentul_4_c
                
    ))
}

