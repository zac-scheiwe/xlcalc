VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "R9Impl"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Implements R9IA
Implements R9IB

Public Tag As String
Private m_v As Variant

Private Sub Class_Initialize()
    R9Log = R9Log & "i"
End Sub

Private Sub Class_Terminate()
    R9Log = R9Log & "x" & Tag
End Sub

Private Function R9IA_Name() As String
    R9IA_Name = "A" & Tag
End Function

Private Sub R9IA_Run(ByVal n As Long)
    R9Log = R9Log & "[A.Run" & n & "]"
End Sub

Private Property Get R9IA_V() As Variant
    R9IA_V = m_v
End Property

Private Function R9IB_Name() As String
    R9IB_Name = "B" & Tag
End Function

Private Function R9IB_Calc(ByRef x As Long) As Long
    x = x * 2
    R9IB_Calc = x + 1
End Function

Private Sub R9IB_Run(ByVal n As Long)
    R9Log = R9Log & "[B.Run" & n & "]"
    Err.Raise 5
End Sub

Public Function Name() As String
    Name = "own" & Tag
End Function

Public Property Let SetV(ByVal x As Variant)
    m_v = x
End Property

Public Property Get Dflt() As String
Attribute Dflt.VB_UserMemId = 0
    Dflt = "dflt" & Tag
End Property
