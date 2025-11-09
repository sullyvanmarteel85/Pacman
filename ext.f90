module librairy
    use iso_c_binding
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
! DECLARATION NOUVEAUX TYPES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type :: remplissage
    integer :: state, Y, X
end type

type :: joueur
    integer :: Y, X, AA, XX
end type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!2.création de la fonction de changement d'etat des remplissages
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

type(remplissage) function changement_etat_remplissage(rempi,rempf)
    implicit none

type(remplissage), intent(in) :: rempi
type(remplissage), intent(out) :: rempf

select case (rempi%state)
    case (2)
        rempf%state = 1
    case (3,4)
        rempf%state = 2
    case default
        rempf%state=rempi%state
end select

end function changement_etat_remplissage

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!2. création de la fonction de changement de score (compteur)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer function score (remp,player,compt)
    implicit none

    type(remplissage), intent(in) :: remp
    type(joueur), intent(in) :: player !variables qui passe de 0 a 1 quand un bonus est mangé ( modifié dans la fonction de complacement)
    integer, intent(inout) :: compt

    integer ::AA,XX

    select case (remp%state)
        case(2)
            compt = compt + 1
        case(3)
            compt = compt + 1
        case(4)
            compt = compt + 1
        case default
            compt = compt + 0
end select

AA=player%AA
XX=player%XX

    if (AA==0 .and. XX==0) then
        if (MOD(compt,15)==0) then
            compt = compt - 1
        end if
    else if (AA==1 .and. XX==0) then
        if (MOD(compt,10)==0) then
            compt = compt - 1
        end if
    else if (XX==1) then
        if (MOD(compt,7)==0) then
            compt = compt - 1
        end if
    end if

end function score

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!2.création de la fonction de changement d'etat du joueur
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(joueur) function changement_etat_joueur(tabremplissage,joueuri,joueurf)
    implicit none

type(joueur), intent(inout) :: joueuri
type(joueur), intent(out) :: joueurf
type(remplissage), dimension(:,:),intent(in):: tabremplissage

integer :: X,Y

joueurf=joueuri
X=joueuri%X
Y=joueuri%Y

select case (tabremplissage(X,Y)%state)
    case (3)
        joueurf%AA=1
    case (4)
        joueurf%XX=1
    case default
        joueurf%AA=joueuri%AA
        joueurf%XX=joueuri%XX
end select

end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!3. affichage tableau remplissage => adaptation de l'affichage dynamique (1) pour le jeu
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine affichage_labyrinthe(tabremplissage)
    implicit none

    type(remplissage), dimension(:,:),intent(inout):: tabremplissage

    integer :: i,j,m,N

    m=27
    N=68

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
end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!2,3. subroutine de deplacement, detection de fleche
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!ID clavier des flèches : >=261 ; <=260 ; ^=259 ; v=258

subroutine deplacement(tabremplissage,player)
    implicit none

type(remplissage), dimension(:,:),intent(inout):: tabremplissage
type(joueur), intent(inout) :: player

integer :: touche,i,j,m,N

    m=27
    N=68
touche=0

do while (touche/=258 .or. touche/=259 .or. touche/=260 .or. touche/=261) ! ne fait rien si la touche préssée n'est pas une fleche
    touche=getch()
    do i=1,m
        do j=1,N
            select case (tabremplissage(i,j)%state)
                case (6)
                    touche=0 ! n'avance pas en cas de mur
                case (5)
                    touche=0
            end select
        end do
    end do
end do

do i=1,m
    do j=1,N
        select case (touche)
            case(258)
                player%X=i+1
                player%Y=j
            case(259)
                player%X=i-1
                player%Y=j
            case(260)
                player%X=i
                player%Y=j-1
            case(261)
                player%X=i
                player%Y=j+1
            case default
                player%X=i
                player%Y=j
        end select
    end do
end do

end subroutine



end module
