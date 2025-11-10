/*
 * Sistema Especialista para Mercado de Carbono - Backend
 * Desenvolvido por: Lucas Gilmar da Silva (@Koyall)
 * Desenvolvido por: Felipe José Sens (@FelipeJoseSens)
 */

% Carrega todos os arquivos necessarios
:- [kb, rules, ui, explain].

% =======================================================
% PONTO DE ENTRADA PRINCIPAL
% =======================================================

% O ponto de entrada que deve ser chamado no console e 'iniciar'.
% Ele apenas chama o predicado 'menu_principal' definido no ui.pl
iniciar :-
    menu_principal. % Chama o menu principal definido em ui.pl

% Predicado de limpeza (mantido do original)
limpar_base :-
    retractall(emissao_setor(_, _)),
    retractall(emissao_total(_)),
    retractall(compensacao_existente(_, _)),
    retractall(meta_reducao(_)),
    retractall(creditos_disponiveis(_)),
    retractall(regra_acionada(_, _)),
    retractall(resultado_final(_)).