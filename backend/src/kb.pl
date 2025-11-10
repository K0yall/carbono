/*
 * kb.pl - Base de Conhecimento do Sistema de Carbono
 * Define fatos, tabelas e fatores de emissao por setor
 */

:- use_module(library(lists)). % CORRIGIDO: Usa 'lists' em vez de 'sumlist'

% Predicados dinamicos (podem ser modificados em tempo de execucao)
:- dynamic emissao_setor/2.
:- dynamic emissao_total/1.
:- dynamic compensacao_existente/2.
:- dynamic meta_reducao/1.
:- dynamic creditos_disponiveis/1.
:- dynamic resultado_final/1.

% Fatores de emissao por setor (tCO2e por R$ 1.000.000)
% Baseado em medias de mercado brasileiro
fator_emissao(energia, 500). % 500 tCO2e por milhao de reais
fator_emissao(transportes, 300). % 300 tCO2e por milhao de reais
fator_emissao(industria, 400). % 400 tCO2e por milhao de reais
fator_emissao(agropecuaria, 600). % 600 tCO2e por milhao de reais
fator_emissao(residuos, 250). % 250 tCO2e por milhao de reais

% Fatores de compensacao por tipo de projeto (tCO2e/ano)
fator_compensacao(reflorestamento, 1.0).
fator_compensacao(energia_renovavel, 0.9).
fator_compensacao(efficiencia_energetica, 0.7).
fator_compensacao(captura_carbono, 1.2).
fator_compensacao(nenhum, 0.0).

% Preco medio de credito de carbono (R$ por tCO2e)
preco_credito(80).

% Calcula emissao de um setor baseado no faturamento
calcular_emissao_setor(Setor, Faturamento, Emissao) :-
    fator_emissao(Setor, Fator),
    Emissao is (Faturamento / 1000000) * Fator.

% Calcula emissao total somando todos os setores
calcular_emissao_total(Total) :-
    findall(E, emissao_setor(_, E), Emissoes),
    sum_list(Emissoes, Total),
    assertz(emissao_total(Total)).

% Calcula compensacao total considerando o fator de eficiencia
calcular_compensacao_total(Total) :-
    (compensacao_existente(Tipo, Quantidade) ->
        fator_compensacao(Tipo, Fator),
        Total is Quantidade * Fator
    ;
        Total = 0
    ).