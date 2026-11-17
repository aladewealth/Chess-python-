import json,os,urllib.request as R
N,E,S,W=-10,1,10,-1
V=dict(P=100,N=310,B=330,R=500,Q=950,K=60000)
D=dict(P=(N,2*N,N+W,N+E),N=(2*N+E,N+2*E,S+2*E,2*S+E,2*S+W,S+2*W,N+2*W,2*N+W),B=(N+E,S+E,S+W,N+W),R=(N,E,S,W))
D['Q']=D['K']=D['B']+D['R']
def bo(p,i):
 r,f=9-i//10,i%10-1;c=abs(2*f-7)+abs(2*r-7)
 return{'P':r*r*2+(7-abs(2*f-7))*3*(r<5)+20*(r==6),'N':40-c*5,'B':20-c*3,'R':20*(r==6)+(7-abs(2*f-7)),'Q':10-c*2,'K':(c*3 if r==0 else-30-r*10)}[p]
T={p:[V[p]+bo(p,i)for i in range(120)]for p in V}
ML=V['K']-9000;MU=V['K']+9000
def pt(b,k,c):return b[:k]+c+b[k+1:]
def rot(s,nl=0):b,sc,wc,bc,ep,kp=s;return(b[::-1].swapcase(),-sc,bc,wc,0 if nl or not ep else 119-ep,0 if nl or not kp else 119-kp)
def gen(s):
 b,_,wc,_,ep,kp=s
 for i,p in enumerate(b):
  if not p.isupper():continue
  for d in D[p]:
   j=i+d
   while 1:
    q=b[j]
    if q.isspace()or q.isupper():break
    if p=='P':
     if d in(N,2*N)and q!='.':break
     if d==2*N and(i<81 or b[i+N]!='.'):break
     if d in(N+W,N+E)and q=='.'and j not in(ep,kp,kp-1,kp+1):break
    yield i,j,'Q'
    if p in'PNK'or q.islower():break
    if i==91 and b[j+E]=='K'and wc[0]:yield j+E,j+W,'Q'
    if i==98 and b[j+W]=='K'and wc[1]:yield j+W,j+E,'Q'
    j+=d
def val(s,m):
 b,_,_,_,ep,kp=s;i,j,r=m;p,q=b[i],b[j];v=T[p][j]-T[p][i]
 if q.islower():v+=T[q.upper()][119-j]
 if kp and abs(j-kp)<2:v+=T['K'][119-j]
 if p=='K'and abs(i-j)==2:v+=T['R'][(i+j)//2]-T['R'][91 if j<i else 98]
 if p=='P':
  if j<29:v+=T[r][j]-T['P'][j]
  if j==ep:v+=T['P'][119-j-S]
 return v
def mv(s,m):
 b,sc,wc,bc,ep,kp=s;i,j,r=m;p=b[i];sc+=val(s,m);b=pt(pt(b,j,p),i,'.');e=k=0
 if i==91:wc=(0,wc[1])
 if i==98:wc=(wc[0],0)
 if j==21:bc=(bc[0],0)
 if j==28:bc=(0,bc[1])
 if p=='K':
  wc=(0,0)
  if abs(j-i)==2:k=(i+j)//2;b=pt(pt(b,91 if j<i else 98,'.'),k,'R')
 if p=='P':
  if j<29:b=pt(b,j,r)
  if j-i==2*N:e=i+N
  if j==ep:b=pt(b,j+S,'.')
 return rot((b,sc,wc,bc,e,k))
H={};C=[0]
class X(Exception):pass
def chk(s):return any(rot(s,1)[0][j]=='k'for _,j,_ in gen(rot(s,1)))
def srch(s,d,a,z,y):
 C[0]+=1
 if C[0]>L[1]:raise X
 if s[1]<=-ML:return-MU+y
 if y and s[0]in Q:return 0
 st=s[1]
 if d<1:
  if st>=z:return st
  a=max(a,st)
 elif d>2 and y and st>=z and not chk(s):
  if-srch(rot(s,1),d-3,-z,1-z,y+1)>=z:return z
 h=H.get(s);ms=sorted(gen(s),key=lambda m:(d>0 and m==h,val(s,m)),reverse=True)
 bs=st if d<1 else-MU;bm=None
 for m in ms:
  if d<1 and val(s,m)<50:break
  c=-srch(mv(s,m),d-1,-z,-a,y+1)
  if c>bs:bs,bm=c,m
  if c>a:a=c
  if a>=z:break
 if bm:H[s]=bm
 if d>0 and bs<=-ML and not chk(s):return 0
 return bs
L=[5e4,3e5]
def think(s):
 C[0]=0;b=None
 for d in range(1,40):
  try:c=srch(s,d,-MU,MU,0)
  except X:break
  b=H.get(s)
  if C[0]>L[0]or abs(c)>=ML:break
 return b or next(gen(s))
def sq(t):return 91+ord(t[0])-97-10*(int(t[1])-1)
def nm(i,w):i=i if w else 119-i;return'abcdefgh'[i%10-1]+str(10-i//10)
def fen(f):
 f=f.split();b=' '*20+''.join(' '+''.join('.'*int(c)if c.isdigit()else c for c in r)+' 'for r in f[0].split('/'))+' '*20
 sc=sum(T[p][i]if p.isupper()else-T[p.upper()][119-i]for i,p in enumerate(b)if p.isalpha())
 c=f[2];s=(b,sc,('Q'in c,'K'in c),('k'in c,'q'in c),0 if f[3]=='-'else sq(f[3]),0)
 return(rot(s),0)if f[1]=='b'else(s,1)
def play(s,w,u):
 i,j=sq(u[:2]),sq(u[2:4])
 if not w:i,j=119-i,119-j
 return mv(s,(i,j,u[4:].upper()or'Q')),1-w
U=os.environ['MAKEFUN_ENV_URL']
def P(p,b):return json.load(R.urlopen(R.Request(U+p,json.dumps(b).encode(),{'content-type':'application/json'})))
Q=set()
while 1:
 g=P('/game',{})
 if g['status']=='done':break
 if g['status']!='your_move':continue
 if len(H)>5e5:H.clear()
 s,w=fen(g['start_fen']);Q={s[0]}
 for u in g['moves']:s,w=play(s,w,u);Q.add(s[0])
 i,j,r=think(s);m=nm(i,w)+nm(j,w)+('q'if s[0][i]=='P'and j<29 else'')
 P('/move',{'game':g['game'],'move':m if m in g['legal_moves']else g['legal_moves'][0]})
