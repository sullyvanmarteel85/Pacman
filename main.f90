
program pacman_game
    use iso_c_binding ! bibliothèque ncurses
    use librairy ! module
    implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DEF VARIABLE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer :: p,er,ert,er2,m,N,i,j,compteur,AA,XX,coins,S,C,comptc,compts
character(50):: name,d,scorefinal
type(c_ptr) :: win
logical :: bump

type(remplissage) :: v, mh, mv, mm, o, a, x, remps, test
type(joueur)pacmanplayer

character(1024):: tampon
type(remplissage), dimension(:,:),allocatable:: tabremplissage

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!SPAWN PACMAN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

pacmanplayer%X=2
pacmanplayer%Y=2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!1. demande et stockage nom du joueur
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

print*,'entrez le nom du joueur'
read(*,*)name
 ! /!\  A METTRE AVANT LE INITSCREEN CAR CMD DE FORTRAN

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!1. exportation des résultats d'une partie
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

open(unit=10,file='score.txt',status='unknown',action='readwrite', position='rewind',iostat=er)

if (er>0) then
    write(*,*)"erreur fichier score"
end if


close(unit=10) ! à mettre a la fin du programme

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!1. affichage dynamique contrôlable
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!win=initscr() !permet de lancer ncurses /!\ NE PAS METTRE EN COMMENTAIRE QUAND UTILISE CAR PERMETS D'UTILISER LES FCT DE LA BIBLIOTHEQUE
!bump = .true.

!do while (1==1)
!    call clear()
!    bump= .not. bump
!    if (bump) then
!        call mvprintw(0,0,'C'//char(0))
!    else
!        call mvprintw(4,4,'C'//char(0))
!    end if
!    call refresh()
!    call sleep(1)
!end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!1. détection et réaction d'appui de touches
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!do while (1==1)
!    call printw('Appuie sur une touche...'//char(0))
!    call cbreak() !rend les touches instantanément disponibles
!    call noecho() !ne les affiches pas
!    call keypad(win,logical(.true.,c_bool)) ! /!\ le logical (bien que defini comme tel dans la fonction) permets d'eviter les erreurs de conversions de tailles d'octet)
!    p=getch() ! lit la touche
!    do while (1==1)
!       call clear()
!       call printw("touche detectee"// char(0))
!        write(d,'(I0)') p
!        call printw(d)
!       call refresh()
!       call sleep(1)
!    end do
!end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!2. création d'un remplissage
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! -> rattaché a #creation type et # def variable

! valeurs des states : vide v=0 ; miette mm= 1 ; coins o=2 ; bonusa a=3 ; bonusX x=4 ; mur verticaux mv=5 ; mur horizontaux mh=6

!v%state = 0
!mh%state = 6
!mv%state = 5
!mm%state = 1
!o%state = 2
!a%state = 3
!x%state = 4

!test = changement_etat_remplissage(mv,remps)

!print*,remps

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!initialisation compteurs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

coins=0
comptc=0
compts=0
S=0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!2. créer un contenant de remplissage (tableau)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

m=27
N=68

allocate(tabremplissage(m+1,N+1),stat=ert)

if (ert/=0) then
    write(*,*)"erreur tableau"
end if

open(unit=11,file='map.txt',status='old',action='read',iostat=er2)

if (er>0) then
        write(*,*)"erreur fichier map"
end if

do i=1,m
        read(11,'(A)')tampon !(A) permet de lire tous les characteres dont les espaces
!        print*,trim(tampon) !trim() permets de reccuperer la chaine de caractere tout pile ( enleve les espaces et caractere en trop a la fin)
        do j=1,N
            select case (tampon(j:j)) !utilise la chaine de charactere comme un tableau
                case('-')
                    tabremplissage(i,j)%state = 6
                case('|')
                    tabremplissage(i,j)%state = 5
                case('o')
                    tabremplissage(i,j)%state = 2
                    coins = coins + 1
                case('a')
                    tabremplissage(i,j)%state = 3
                    coins = coins + 1
                case('x')
                    tabremplissage(i,j)%state = 4
                    coins = coins + 1
                case('.')
                    tabremplissage(i,j)%state = 1
                case default
                    tabremplissage(i,j)%state = 0
            end select
        end do
end do

close(unit=11)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!initialisation de l'écran
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

win=initscr()
call cbreak()
call noecho()
call keypad(win,logical(.true.,c_bool))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!squelette
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call affichage(tabremplissage,pacmanplayer)
do while (coins>0)
    call deplacement(tabremplissage,pacmanplayer)
    S=score(pacmanplayer,comptc,S)
    call changement_etat (tabremplissage,pacmanplayer,coins)
    call affichage(tabremplissage,pacmanplayer)
end do

!call clear()
!write(scorefinal,'(I0)') S
!call mvprintw(10,33,"score:"//char(0))
!call mvprintw(13,34,scorefinal//char(0))
!call refresh()
!call sleep(5)

write(scorefinal,'(I0)') S
bump = .false.

do while (1==1)
    call clear()
    bump= .not. bump
    call mvprintw(10,33,"score:"//char(0))
    if (bump) then
        call mvprintw(13,34,scorefinal//char(0))
    end if
    call refresh()
    call usleep(300000)
end do

end program pacman_game
