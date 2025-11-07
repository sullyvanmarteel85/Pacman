module librairy
    implicit none


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DECLARATION NOUVEAU TYPE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type :: remplissage
    integer :: state, Y, X
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


integer function score (remp,AA,XX,compt)
    implicit none

    type(remplissage), intent(in) :: remp
    integer, intent(in) :: AA,XX !variables qui passe de 0 a 1 quand un bonus est mangé ( modifié dans la fonction de complacement)
    integer, intent(inout) :: compt

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

end module
