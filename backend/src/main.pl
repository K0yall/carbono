/* 
 * Sistema Especialista para Mercado de Carbono - Backend
 * Desenvolvido por: [Seu Nome] @seu_usuario
 */

:- [kb, rules, ui, explain].

% Limpa fatos dinâmicos antes de nova consulta
limpar_base :-
    retractall(emissao_setor(_, _)),
    retractall(emissao_total(_)),
    retractall(compensacao_existente(_, _)),
    retractall(meta_reducao(_)),
    retractall(creditos_disponiveis(_)),
    retractall(regra_acionada(_, _)),
    retractall(resultado_final(_)).