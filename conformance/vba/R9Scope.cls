VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "R9Scope"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private m_n As Long
Friend Property Get FN() As Long
    FN = m_n
End Property
Friend Property Let FN(ByVal x As Long)
    m_n = x
End Property
Friend Function FF(ByVal x As Long) As Long
    FF = x + m_n
End Function
Private Function PF(ByVal x As Long) As Long
    PF = x * 3
End Function
Public Function PubCallsPriv(ByVal x As Long) As Long
    PubCallsPriv = PF(x) + FF(1)
End Function
Public Function ViaMe(ByVal x As Long) As Long
    ViaMe = Me.PubCallsPriv(x) + Me.FF(0)
End Function
Public Function Item(ByVal i As Long) As String
Attribute Item.VB_UserMemId = 0
    Item = "item" & i & m_n
End Function
Public Function Cnt() As Long
    m_n = m_n + 1
    Cnt = m_n
End Function
