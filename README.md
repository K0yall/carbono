# 🌿 Sistema Especialista em Mercado de Carbono

**Trabalho 03 - Programação Lógica (UNIDAVI - Prof. Esp. Ademar Perfoll Junior)**

Este sistema especialista, implementado em **SWI-Prolog**, realiza a **análise de compliance e risco climático** de uma empresa. O sistema deduz o **status de conformidade** (compliant/não compliant), calcula a **necessidade/excedente de créditos de carbono** e fornece uma **recomendação estratégica de investimento**, baseando-se em fatos e regras de negócio.

## 👥 Desenvolvedores

* **Lucas Gilmar da Silva** (@K0yall)
* **Felipe José Sens** (@FelipeJoseSens)

---

▶️ Como Executar
O sistema é executado diretamente via interface de console do SWI-Prolog.

Execução no Terminal
Navegue até a pasta src/:

Bash

cd [caminho_para_o_projeto]/src
Inicie e carregue o programa (via main.pl):

Bash

swipl -s main.pl
Execute o ponto de entrada iniciar:

Prolog

?- iniciar.
Isso carregará o menu principal para iniciar a consulta.

📝 Exemplos de Cenários Analisados
O sistema testa a lógica de compliance, compensação e déficit de créditos de carbono:

Cenário 1: Empresa em Compliance

Resultado: STATUS: COMPLIANT ou COMPLIANT_COM_CREDITOS

Lógica: Emissão líquida (após compensação) está abaixo do limite permitido pela meta. Gera excedente de créditos ou atinge a meta com precisão.

Cenário 2: Empresa Não Compliant

Resultado: STATUS: NAO_COMPLIANT

Lógica: Emissão líquida excede a meta. O sistema calcula o déficit necessário de créditos e o investimento (custo de compra) para cobrir a diferença.

✅ Regras de Negócio Implementadas (Rules.pl)
O sistema utiliza 9 regras principais de inferência para determinar o resultado final e a classificação de risco:

Cálculo de Emissão Líquida (Regra 1)

Definição da Meta e Emissão Permitida (Regra 2)

Verificação de Compliance (Regra 3, 4)

Avaliação de Créditos para cobrir Déficit (Regra 5, 6)

Cálculo de Investimento Necessário (Regra 8)

Classificação de Risco da Empresa (Regra 7.1 a 7.6)

Recomendação de Projetos (Regra 9)