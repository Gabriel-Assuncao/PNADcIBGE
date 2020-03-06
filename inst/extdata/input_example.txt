/* PROGRAMA DE LEITURA EM SAS DO ARQUIVO DE MICRODADOS DA PNAD CONTÍNUA */

/* Obs.1: As duas primeiras posições da UPA (Unidade Primária de Amostragem) são o código da UF (Unidade da Federação)

   Obs.2: Ajuste o endereço do arquivo \PNADC_xxxx.txt no comando INFILE */


data pnadc_xxxxxx;
infile "...\Dados\PNADC_xxxxxx.txt" /* PROGRAMA DE LEITURA EM SAS DO ARQUIVO DE MICRODADOS DA PNAD CONTÍNUA */

/* Obs.1: As duas primeiras posições da UPA (Unidade Primária de Amostragem) são o código da UF (Unidade da Federação)

   Obs.2: Ajuste o endereço do arquivo \PNADC_xxxx.txt no comando INFILE */


data pnadc_xxxxxx;
infile "...\Dados\PNADC_xxxxxx.txt" lrecl=500 Missover;
input
@0001 Ano      $4.    /* Ano de referência */
@0005 Trimestre      $1.    /* Trimestre de referência */
@0006 UF      $2.    /* Unidade da Federação */
@0008 Capital      $2.    /* Município da Capital */
@0010 RM_RIDE      $2.    /* Reg. Metr. e Reg. Adm. Int. Des. */
@0012 UPA      $9.    /* Unidade Primária de Amostragem */
@0021 Estrato      $7.    /* Estrato */
@0028 V1008      $2.    /* Número de seleção do domicílio */
@0030 V1014      $2.    /* Painel */
@0032 V1016      $1.    /* Número da entrevista no domicílio */
@0033 V1022      $1.    /* Tipo de situação da região */
@0034 V1023      $1.    /* Tipo de área */
@0035 V1027       15.    /* Peso SEM pós estratificação */
@0050 V1028       15.    /* Peso COM pós estratificação */
@0065 V1029       9.    /* Projeção da população */
@0074 posest      $3.    /* Domínios de projeção */
@0077 V2001       2.    /* Número de pessoas no domicílio */
@0079 V2003      $2.    /* Número de ordem */
@0081 V2005      $2.    /* Condição no domicílio */
@0083 V2007      $1.    /* Sexo */
@0084 V2008      $2.    /* Dia de nascimento */
@0086 V20081      $2.    /* Mês de nascimento */
@0088 V20082      $4.    /* Ano de nascimento */
@0092 V2009       3.    /* Idade na data de referência */
@0095 V2010      $1.    /* Cor ou raça */
@0096 V3001      $1.    /* Sabe ler e escrever */
@0097 V3002      $1.    /* Frequenta escola */
@0098 V3002A      $1.    /* A escola que ... frequenta é de */
@099 V3003      $2.    /* Qual é o curso que frequenta */
@0101 V3003A      $2.    /* Qual é o curso que frequenta */
@0103 V3004      $1.    /* Duração deste curso que requenta */
@0104 V3005      $1.    /* Curso que frequenta é seriado */
@0105 V3005A      $1.    /* Curso que freq é organizado em: */
@0106 V3006      $2.    /* Qual é o ano/série que frequenta */
@0108 V3006A      $1.    /* Qual é a etapa que frequenta */
@0109 V3007      $1.    /* Concluiu outro curso de graduação */
@0110 V3008      $1.    /* Anteriormente frequentou escola */
@0111 V3009      $2.    /* Curso mais elevado que frequentou */
@0113 V3009A      $2.    /* Curso mais elevado que frequentou */
@0115 V3010      $1.    /* Duração do curso que frequentou */
@0116 V3011      $1.    /* Curso quefrequentou era seriado */
@0117 V3011A      $1.    /* Curso que freq é organizado em: */
@0118 V3012      $1.    /* Aprovado na prim. série do curso */
@0119 V3013      $2.    /* Último ano/série que concluiu */
@0121 V3013A      $1.    /* Qual é a etapa que frequentou */
@0122 V3013B      $1.    /* Cursou os anos iniciais deste curso */
@0123 V3014      $1.    /* Concluiu o curso que frequentou */
@0124 V4001      $1.    /* Trabalhou 1 hr em ativ. remunerd. */
@0125 V4002      $1.    /* Trabalhou 1 hr em produtos etc... */
@0126 V4003      $1.    /* Fez algum bico pelo menos de 1 hr */
@0127 V4004      $1.    /* Ajudou sem receber no domic. 1 hr */
@0128 V4005      $1.    /* Afastado trabalho remunerado */
@0129 V4006      $1.    /* Motivo de estar afastado */
@0130 V4006A      $1.    /* Motivo de estar afastado */
@0131 V4007      $1.    /* Durante afastameno recebia pagam. */
@0132 V4008      $1.    /* Quanto tempo que estava afastado */
@0133 V40081      $2.    /* Tempo de afastamenento até 1 ano */
@0135 V40082      $2.    /* Tempo de afastamen. de 1 a 2 anos */
@0137 V40083      $2.    /* Tempo de afastamen. mais d 2 anos */
@0139 V4009      $1.    /* Quantos trabalhos tinhana semana */
@0140 V4010      $4.    /* Ocupação no trab. principal */
@0144 V4012      $1.    /* Posição na ocupação */
@0145 V40121      $1.    /* Tipo trabalhador não remunerado */
@0146 V4013      $5.    /* Atividade no trab. principal */
@0151 V40132      $1.    /* Seção da atividade */
@0152 V40132A      $1.    /* Seção da atividade */
@0153 V4014      $1.    /* Esse trabalho era na área */
@0154 V4015      $1.    /* Teve ajuda de pelo menos um trabalhador não remunerado */
@0155 V40151      $1.    /* Qnts trabalhadores não remunerados */
@0156 V401511      $1.    /* 1 a 5 trabalhadores não remunerados */
@0157 V401512      $2.    /* 6 a 10 trabalhadores não remunerados */
@0159 V4016      $1.    /* Qnts empregados trabalhavam nesse negócio/empresa*/
@0160 V40161      $1.    /* 1 a 5 empregados */
@0161 V40162      $2.    /* 6 a 10 empregados */
@0163 V40163      $2.    /* 11 a 50 empregados */
@0165 V4017      $1.    /* Tinha pelo menos um sócio que trab. nesse negócio/empresa*/
@0166 V40171      $1.    /* Quantos sócios */
@0167 V401711      $1.    /* 1 a 5 sócios */
@0168 V4018      $1.    /* Qnts pessoas trabalhavam nesse negócio/empresa */
@0169 V40181      $1.    /* 1 a 5pessoas */
@0170 V40182      $2.    /* 6 a 10pessoas */
@0172 V40183      $2.    /* 11 a 50pessoas */
@0174 V4019      $1.    /* Negócio/empresa registrado no CNPJ*/
@0175 V4020      $1.    /* Em que tipo de local funcionava esse negócio/empresa*/
@0176 V4021      $1.    /* Exercia o trabalho em estabelecimento desse negócio/empresa*/ 
@0177 V4022      $1.    /* Onde exercia normalmente esse trabalho*/
@0178 V4024      $1.    /* Serv. domést. em mais de 1 domic. */
@0179 V4025      $1.    /* Contratado como empreg. temporário*/
@0180 V4026      $1.    /* Era contratado somente por pessoa responsável pelo negócio */
@0181 V4027      $1.    /* Era contratado somente por intermediário */
@0182 V4028      $1.    /* Servidor público estatutário */
@0183 V4029      $1.    /* Carteira de trabalho assinada */
@0184 V4032      $1.    /* Contribuinte de instit. d previd. */
@0185 V4033      $1.    /* Rendimento habitual var. auxil. */
@0186 V40331      $1.    /* Rendimento habitual em dinheiro */
@0187 V403311      $1.    /* Faixa do valor do rendimento hab. */
@0188 V403312       8.    /* Valor do rend. hab. em dinheiro */
@0196 V40332      $1.    /* Rendimento habitual em produtos */
@0197 V403321      $1.    /* Faixa do valor do rendimento hab. */
@0198 V403322       8.    /* Valor do rend. hab. em produtos */
@0206 V40333      $1.    /* Rendimento habitual em benefícios */
@0207 V403331      $1.    /* Tipo rend. habitual em benefícios */
@0208 V4034      $1.    /* Rendimento efetivo var. auxil. */
@0209 V40341      $1.    /* Rendimento efetivo em dinheiro */
@0210 V403411      $1.    /* Faixa do valor do rendimento efe. */
@0211 V403412       8.    /* Valor do rend. efe. em dinheiro */
@0219 V40342      $1.    /* Rendimento efetivo em produtos */
@0220 V403421      $1.    /* Faixa do valor do rendimento efe. */
@0221 V403422       8.    /* Valor do rend. efe. em produtos */
@0229 V4039       3.    /* Hrs habituais no trab. princ. */
@0232 V4039C       3.    /* Hrs efetivas no trab. princ. */
@0235 V4040      $1.    /* Tempo que estava nesse trabalho */
@0236 V40401       2.    /* De 1 mês a menos de 1 ano */
@0238 V40402       2.    /* De 1 ano a menos de 2 anos */
@0240 V40403       2.    /* De 2 anos ou mais tempo */
@0242 V4041      $4.    /* Ocupação no trab. secundário */
@0246 V4043      $1.    /* Posição na ocupação */
@0247 V40431      $1.    /* Tipo trabalhador não remunerado */
@0248 V4044      $5.    /* Atividade no trab. secundário */
@0253 V4045      $1.    /* Esse trabalho era na área */
@0254 V4046      $1.    /* Negócio/empresa registrado no CNPJ*/
@0255 V4047      $1.    /* Servidor público estatutário */
@0256 V4048      $1.    /* Carteira de trabalho assinada */
@0257 V4049      $1.    /* Contribuinte de instit. d previd. */
@0258 V4050      $1.    /* Rendimento habitual var. auxil. */
@0259 V40501      $1.    /* Rendimento habitual em dinheiro */
@0260 V405011      $1.    /* Faixa do valor do rendimento hab. */
@0261 V405012       8.    /* Valor do rend. hab. em dinheiro */
@0269 V40502      $1.    /* Rendimento habitual em produtos */
@0270 V405021      $1.    /* Faixa do valor do rendimento hab. */
@0271 V405022       8.    /* Valor do rend. hab. em produtos */
@0279 V40503      $1.    /* Rendimento habitual em benefícios */
@0280 V405031      $1.    /* Tipo rend. habitual em benefícios */
@0281 V4051      $1.    /* Rendimento efetivo var. auxil. */
@0282 V40511      $1.    /* Rendimento efetivo em dinheiro */
@0283 V405111      $1.    /* Faixa do valor do rendimento efe. */
@0284 V405112       8.    /* Valor do rend. efe. em dinheiro */
@0292 V40512      $1.    /* Rendimento efetivo em produtos */
@0293 V405121      $1.    /* Faixa do valor do rendimento efe. */
@0294 V405122       8.    /* Valor do rend. efe. em produtos */
@0302 V4056       3.    /* Hrs habituais no trab. secun. */
@0305 V4056C       3.    /* Hrs efetivas no trab. secun. */
@0308 V4057      $1.    /* Contribuinte de instit. d previd. */
@0309 V4058      $1.    /* Rendimento habitual var. auxil. */
@0310 V40581      $1.    /* Rendimento habitual em dinheiro */
@0311 V405811      $1.    /* Faixa do valor do rendimento hab. */
@0312 V405812       8.    /* Valor do rend. hab. em dinheiro */
@0320 V40582      $1.    /* Rendimento habitual em produtos */
@0321 V405821      $1.    /* Faixa do valor do rendimento hab. */
@0322 V405822       8.    /* Valor do rend. hab. em produtos */
@0330 V40583      $1.    /* Rendimento habitual em benefícios */
@0331 V405831      $1.    /* Tipo rend. habitual em benefícios */
@0332 V40584      $1.    /* Não remunerado */
@0333 V4059      $1.    /* Rendimento efetivo var. auxil. */
@0334 V40591      $1.    /* Rendimento efetivo em dinheiro */
@0335 V405911      $1.    /* Faixa do valor do rendimento efe. */
@0336 V405912       8.    /* Valor do rend. efe. em dinheiro */
@0344 V40592      $1.    /* Rendimento efetivo em produtos */
@0345 V405921      $1.    /* Faixa do valor do rendimento efe. */
@0346 V405922       8.    /* Valor do rend. efe. em produtos */
@0354 V4062       3.    /* Hrs habituais no(s) outro(s) trab.*/
@0357 V4062C       3.    /* Hrs efetivas no(s) outro(s) trab .*/
@0360 V4063      $1.    /* Gostaria trabalhar + hrs efetivas */
@0361 V4063A      $1.    /* Gostaria trabalhar + hrs habituais*/
@0362 V4064      $1.    /* Dispon. trabalhar + hrs efetivas */
@0363 V4064A      $1.    /* Dispon. trabalhar + hrs habituais */
@0364 V4071      $1.    /* Providência p/ conseg. trab.(30d) */
@0365 V4072      $2.    /* Principal provid. p/conseg. trab. */
@0367 V4072A      $1.    /* Principal provid. p/conseg. trab. */
@0368 V4073      $1.    /* Gostaria de ter trabalhado */
@0369 V4074      $1.    /* Motivo de não ter tomado provid. */
@0370 V4074A      $2.    /* Motivo de não ter tomado provid. */
@0372 V4075A      $1.    /* Tempo em que irá começar o trab. */
@0373 V4075A1      $2.    /* Meses em que irá começar o trab. */
@0375 V4076      $1.    /* Tempo tentando conseguir trabalho */
@0376 V40761       2.    /* Tempo tentando trab. 1 mes-1 ano */
@0378 V40762       2.    /* Tempo tentando trab. 1 ano-2 anos */
@0380 V40763       2.    /* Tempo tentando trab. + de 2 anos */
@0382 V4077      $1.    /* Poderia ter começado a trabalhar */
@0383 V4078      $1.    /* Motivo p/ñ querer/começar a trab. */
@0384 V4078A      $1.    /* Motivo p /ñ querer/começar a trab. */
@0385 V4082      $1.    /* Trab por pelo menos 1 hora em 1 ano*/
@0386 VD2002      $2.    /* Condição no domicílio */
@0388 VD2003       2.    /* Número de componentes do domic. */
@0390 VD2004      $1.    /* Espécie da unidade doméstica*/
@0391 VD3004      $1.    /* Nível de instrução mais elevado alcançado (5 anos ou mais de idade) */
@0392 VD3005      $2.    /* Anos de estudo (5 anos ou mais de idade) para fundamental de 9 anos */
@0394 VD3006      $1.    /* Grupamento de anos de estudo (pessoas de 5 anos ou mais de idade) para fundamental de 9 anos */
@0395 VD4001      $1.    /* Condição em relação força d trab. */
@0396 VD4002      $1.    /* Condição de ocupação */
@0397 VD4003      $1.    /* Força de trabalho potencial */
@0398 VD4004      $1.    /* Subocupação por insuficiên. de hrs efet*/
@0399 VD4004A      $1.    /* Subocupação por insuficiên. de hrs hab*/
@0400 VD4005      $1.    /* Pessoas desalentadas */
@0401 VD4007      $1.    /* Posição na ocupação trab. princ. */
@0402 VD4008      $1.    /* Posição na ocupação trab. princ. */
@0403 VD4009      $2.    /* Posição na ocupação trab. princ. */
@0405 VD4010      $2.    /* Grupamen. d ativid. trab. princ. */
@0407 VD4011      $2.    /* Grupamen. ocupacion. trab. Princ. */
@0409 VD4012      $1.    /* Contrib. instit. previd. qq trab. */
@0410 VD4013      $1.    /* Faixa hrs habituais em todos trab. */
@0411 VD4014      $1.    /* Faixa hrs efetivas em todos trab. */
@0412 VD4015      $1.    /* Tipo d remuneração trab. princ. */
@0413 VD4016       8.    /* Rendim. habitual trab. princ. */
@0421 VD4017       8.    /* Rendim. efetivo trab. princ. */
@0429 VD4018      $1.    /* Tipo d remuneração em qq trabalho */
@0430 VD4019       8.    /* Rendim. habitual qq trabalho */
@0438 VD4020       8.    /* Rendim. efetivo qq trabalho */
@0446 VD4023      $1.    /* Pq ñ proc./ñ gost./ñ disp.p/trab. */
@0447 VD4030      $1.    /* Pq ñ proc./ñ gost./ñ disp.p/trab. */
@0448 VD4031       3.    /* Hrs habituais em todos trab. */
@0451 VD4032       3.    /* Hrs efetivas no trab. princ. */
@0454 VD4033       3.    /* Hrs efetivas no trab. secun. */
@0457 VD4034       3.    /* Hrs efetivas no(s) outro(s) trab .*/
@0460 VD4035       3.    /* Hrs efetivas em todos trab. */
@0463 VD4036      $1.    /* Faixa hrs habituais trab. princ. */
@0464 VD4037      $1.    /* Faixa hrs efetivas trab. princ. */
;
run;