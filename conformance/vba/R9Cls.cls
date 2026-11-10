VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "R9Cls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private Type LocT
    A As Long
    B As String
End Type

Private m_pt As R9Pt
Private m_arr(1 To 3) As R9Pt
Private m_dyn() As R9Rec
Private m_loc As LocT
Public Tag As String

Private Sub Class_Initialize()
    R9Log = R9Log & "I"
    m_pt.X = 1
    m_loc.A = 5
End Sub

Private Sub Class_Terminate()
    R9Log = R9Log & "T" & Tag
End Sub

Public Property Get Pt() As R9Pt
    Pt = m_pt
End Property

Public Property Let Pt(ByRef p As R9Pt)
    m_pt = p
End Property

Public Sub SetPt(ByRef p As R9Pt)
    m_pt = p
End Sub

Public Sub BumpIt(ByRef p As R9Pt)
    p.X = p.X + m_pt.X
    p.S = p.S & "c"
End Sub

Public Function GetPt() As R9Pt
    GetPt = m_pt
End Function

Public Sub SetEl(ByVal i As Long, ByRef p As R9Pt)
    m_arr(i) = p
End Sub

Public Function ElStr(ByVal i As Long) As String
    ElStr = m_arr(i).X & ":" & m_arr(i).Y & ":" & m_arr(i).S
End Function

Public Sub BumpEl(ByVal i As Long)
    m_arr(i).X = m_arr(i).X + 1
    BumpPt m_arr(i)
End Sub

Public Sub Grow(ByVal n As Long)
    ReDim Preserve m_dyn(1 To n)
    m_dyn(n).Id = n
    m_dyn(n).Nm = "g" & n
End Sub

Public Function DynStr() As String
    DynStr = RecArrStr(m_dyn)
End Function

Public Function LocStr() As String
    m_loc.B = m_loc.B & "x"
    LocStr = m_loc.A & m_loc.B & Len(m_loc)
End Function

Public Function WithTest() As String
    With m_pt
        .X = .X + 10
        With m_arr(2)
            .S = "w"
            .X = m_pt.X
        End With
        WithTest = .X & m_arr(2).S & m_arr(2).X
    End With
End Function
