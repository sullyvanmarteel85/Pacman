
program pacman_game
    use iso_c_binding ! bibliothèque ncurses
    use librairy ! module
    implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! IMPORTATION FONCTIONS BIBLIOTHEQUE NCURSES ( INTERFACE )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface

     function initscr() bind(C, name="initscr") ! initialise l'ecran pour utiliser l'affichage ncurses
       use iso_c_binding
       type(c_ptr) :: initscr
     end function

     subroutine endwin() bind(C, name="endwin") ! remets le terminal hors ncurses
     end subroutine

     subroutine printw(msg) bind(C, name="printw") ! affiche une chaine à la position du curseur
       use iso_c_binding
       character(kind=c_char), dimension(*) :: msg
     end subroutine

     subroutine mvprintw(y, x, fmt) bind(C, name="mvprintw") ! affiche une chaîne à une position donnée
        import :: c_int, c_char
        integer(c_int), value :: y, x
        character(kind=c_char), dimension(*) :: fmt
     end subroutine
     ! /!\ coord en (y,x) et non en (x,y)

     subroutine refresh() bind(C, name="refresh") ! met à jour l’écran avec les changements
     end subroutine

     subroutine clear() bind(C, name="clear") ! fface le contenu de l’écran
     end subroutine

     subroutine usleep(usec) bind(C, name="usleep") ! mets l'ecran en pause ( utiliser sleep() si des secondes suffisent)
        import :: c_int
        integer(c_int), value :: usec
     end subroutine
    !usleep est en microsecondes

    subroutine cbreak() bind(C, name="cbreak") ! active la lecture immédiate du clavier
    end subroutine

    subroutine noecho() bind(C, name="noecho") ! empêche l’affichage des caractères tapés
    end subroutine

    integer(c_int) function getch() bind(C, name="getch") ! lit une touche clavier sans attendre Entrée
        import :: c_int
    end function

    subroutine keypad(win, bf) bind(C, name="keypad") ! permet de lire le keypad
        import :: c_ptr, c_bool
        type(c_ptr), value :: win
        logical(c_bool), value :: bf
    end subroutine

  end interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DEF VARIABLE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer :: Xpac,Ypac,er,ert,er2,m,N,i,j,compteur,AA,XX
character :: pacman
character(50):: name,d
type(c_ptr) :: win
logical :: bump

type(remplissage) :: v, mh, mv, mm, o, a, x, remps, test

character(1024):: tampon
type(remplissage), dimension(:,:),allocatable:: tabremplissage

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEF PACMAN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Xpac=0
Ypac=0
pacman='C'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!1. demande et stockage nom du joueur
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!print*,'entrez le nom du joueur'
!read(*,*)name
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
                case('a')
                    tabremplissage(i,j)%state = 3
                case('x')
                    tabremplissage(i,j)%state = 4
                case('.')
                    tabremplissage(i,j)%state = 1
                case default
                    tabremplissage(i,j)%state = 0
            end select
        end do
end do

close(unit=11)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!3. affichage tableau remplissage => adaptation de l'affichage dynamique (1) pour le jeu
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

win=initscr()

do while (1==1)
    call clear()
    do i=1,m
        do j=1,N
            select case (tabremplissage(i,j)%state )
                case (6)
                    call mvprintw(i,j,'-'//char(0))
                case (5)
                    call mvprintw(i,j,'|'//char(0))
                case (4)
                    call mvprintw(i,j,'x'//char(0))
                case (3)
                    call mvprintw(i,j,'a'//char(0))
                case (2)
                    call mvprintw(i,j,'o'//char(0))
                case (1)
                    call mvprintw(i,j,'.'//char(0))
                case default
                    call mvprintw(i,j,' '//char(0))
            end select
        end do
    end do
    call refresh()
    call usleep(1000000)
end do

end program pacman_game
