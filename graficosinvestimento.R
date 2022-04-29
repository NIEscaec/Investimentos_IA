### primeiramente, para realizar os gráficos, é necessaário criar o data frame
### geral. Para isso, é necessário rodar os seguintes scripts na respectiva ordem:
### 1° IDE; 2° IDP; 3°InterciaPassivo; 4° InvBrap 5° InvEstrp.

library(tidyverse)
library(ggthemes)
library(scales)
install.packages("rccdates")
library(rccdates)
library(zoo)
install.packages("scales")
library(scales)
install.packages("shades")
library(shades)

install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("ggrepel")
library(ggrepel)

país <- "Alemanha"


#### INVESTIMENTOS DO PAÍS NO BRASIL ####
df_grafico0<- DF_Geral %>%
  select(Pais, Ano, b = "IDP_Invest_Imediato", c = "IDP_Control_Final", d = "IDP_Oper_Intercomp", e = "InvEstrp_fluxo_Invest", f = "Inter_Passivo_Ingressos", g = "Inter_Passivo_Amortizacoes")%>%
  filter(Pais == país)%>%
  mutate( Ano = as.numeric(Ano),
          b = as.numeric(b),
          c = as.numeric(c),
          d = as.numeric(d),
          e = as.numeric(e),
          f = as.numeric(f),
          g = as.numeric(g), 
          h = f - g)

qanos <- nrow(df_grafico0)

ggplot(df_grafico0, aes(x = Ano ))+
  geom_bar(stat = "identity", aes(y = c, fill = "IDP - Participação no Capital (Controlador Final)"), position = position_nudge(x = -.20), width = .2)+
  geom_bar(stat = "identity", aes(y = d, fill = "IDP - Operações Intercompanhia"), position = position_nudge(x = -0), width = .2)+
  geom_bar(stat = "identity", aes(y = b, fill = "IDP - Participação no Capital (Invest. Imediato)"), position = position_nudge(x = .20), width = .2)+
  geom_line(aes(y = e*3, color = "Fluxo - Participação no Capital (Invest. Imediato)"), size = 1, linetype = 1) +
  geom_line(aes(y = h*3, color = "Fluxo Líquido - Operações Intercompanhia"), size = 1, linetype = 1) +
  scale_y_continuous("US$ milhões", sec.axis = sec_axis(~ . /3 ))+
  scale_x_yearmon(NULL, format = "%Y", n = qanos)+
  scale_color_manual(NULL, values = saturation(c("#BF1B0B","#38170B","#252A52","#FFC465","#66ADE5"), scalefac(0.8)))+
  scale_fill_manual(NULL, values = saturation(c("#252A52","#FFC465","#66ADE5","#BF1B0B","#38170B"), scalefac(0.8)))+
  theme_classic ()+
  theme(panel.grid = element_blank(), # remove as linhas do corpo do gráfico
        # sem bordas entre os painéis
        panel.spacing = unit(0, "cm"),
        # modifica o texto dos eixos
        axis.text = element_text(size = 12, colour = "black"),
        # cor dos marcadores
        axis.ticks = element_line(colour = "black"),
        # tamanho dos marcadores
        axis.ticks.length = unit(.2, "cm"), 
        #cor da borda
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position="bottom", legend.box = "vertical") 

#### Setor de atividade econômica (Estoque 2020 - US$ milhões) PIZZA ####



AA <- DF_IDP_Por_Setor_Control_Final %>%
  pivot_wider(names_from = "ANO: 2020", values_from = "IDP_Setor_Control_Final") %>%
  filter(Pais == país)%>%
  group_by("ANO: 2020",Pais ) %>% 
  mutate_all(coalesce, 0)

AA<-AA %>%
  select(Pais, b = "B - Indústrias Extrativas", c = "G - Comércio, Reparação de Veículos Automotores e Motocicletas",
         d = "D - Eletricidade e Gás", e = "C - Indústrias de Transformação",
         f = "K - Atividades Financeiras, de Seguros e Serviços Relacionados", g = "H - Transporte, Armazenagem e Correio",
         h = "Total país em US$ milhões")%>%
  mutate( "Indústrias Extrativas" = b*100/h,
          "Comércio, Reparação de Veículos Automotores e Motocicletas" = c*100/h,
          "Eletricidade e Gás" = d*100/h,
          "Indústrias de Transformação" = e*100/h,
          "Atividades Financeiras, de Seguros e Serviços Relacionados" = f*100/h,
          "Transporte, Armazenagem e Correio" = g*100/h,
          "Outros" = (h-b-c-d-e-f-g)*100/h)%>%
  pivot_longer(cols = c("Indústrias Extrativas":"Outros"), names_to = "Setor", values_to = "value")

  
AA %>%
  ggplot(aes(x="", y = value, fill= Setor), label = value)+
    geom_bar(stat = "identity")+
    coord_polar(theta = "y", start = 0)+
    theme_void()+
    geom_label(label = paste(round(AA$value,2),"%"), show.legend = F, size = 5, aes(label = y), position =  position_stack(vjust = 0.5))

#### INVESTIMENTOS BRASILEIROS NO PAÍS X ####
df_grafico1<- DF_Geral %>%
  select(Pais, Ano, b = "IDE_Invest_Imediato", c = "IDE_Oper_Intercomp", d = "IDE_Fluxo_Invest_Imediato")%>%
  filter(Pais == país)%>%
  mutate( Ano = as.numeric(Ano),
          b = as.numeric(b),
          c = as.numeric(c),
          d = as.numeric(d))

qanos<- nrow(df_grafico1)

ggplot(df_grafico1, aes(x = Ano, y = b & c & d )) +
      geom_bar(stat = "identity", aes(y = b, fill = "IBD - Participação no Capital (Invest. Imediato)" ), position = position_nudge(x = -.15), width = .3)+
      geom_bar(stat = "identity", aes(y = c, fill = "IBD - Operações Intercompanhia"  ), position = position_nudge(x = .15), width = .3) +
      geom_line(aes(y = d*10, colour = "Fluxo - Participação no Capital (Invest. Imeadiato)"), size = 1, linetype = 1) +
      scale_y_continuous("US$ milhões", sec.axis = sec_axis(~ . /10 ))+
      scale_x_yearmon(NULL, format = "%Y", n = qanos)+
      scale_color_manual(NULL, values = saturation(c("#BF1B0B","#252A52","#FFC465"), scalefac(0.8)))+
      scale_fill_manual(NULL, values = saturation(c("#252A52","#FFC465","#BF1B0B"), scalefac(0.8)))+
      theme_classic ()+
      theme(panel.grid = element_blank(), # remove as linhas do corpo do gráfico
          # sem bordas entre os painéis
          panel.spacing = unit(0, "cm"),
          # modifica o texto dos eixos
          axis.text = element_text(size = 12, colour = "black"),
          # cor dos marcadores
          axis.ticks = element_line(colour = "black"),
          # tamanho dos marcadores
          axis.ticks.length = unit(.2, "cm"), 
          #cor da borda
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          legend.position=) 




#### INVESTIMENTO EM CARTEIRA ####
df_grafico2<- DF_Geral %>%
    select(Pais, Ano, b = "IDE_Acoes")%>%
    filter(Pais == país)%>%
    mutate( Ano = as.numeric(Ano),
            b = as.numeric(b))
  
qanos<- nrow(df_grafico2)
  
ggplot(df_grafico2, aes(x = Ano, y = b)) +
    geom_bar(stat = "identity", aes(y = b, fill = "Ações" ), width = .5)+
    scale_y_continuous("US$ milhões")+
    scale_x_yearmon(NULL, format = "%Y", n = qanos)+
    scale_fill_manual(NULL, values = saturation(c("#252A52","#FFC465","#BF1B0B"), scalefac(0.8)))+
    theme_classic ()+
    theme(panel.grid = element_blank(), # remove as linhas do corpo do gráfico
          # sem bordas entre os painéis
          panel.spacing = unit(0, "cm"),
          # modifica o texto dos eixos
          axis.text = element_text(size = 12, colour = "black"),
          # cor dos marcadores
          axis.ticks = element_line(colour = "black"),
          # tamanho dos marcadores
          axis.ticks.length = unit(.2, "cm"), 
          #cor da borda
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          legend.position=)   
  
#### OUTROS INVESTIMENTOS ####

df_grafico3<- DF_Geral %>%
  select(Pais, Ano, b = "IDE_Moedas", c = "IDE_Imoveis")%>%
  filter(Pais == país)%>%
  mutate( Ano = as.numeric(Ano),
          b = as.numeric(b),
          c = as.numeric(c))

qanos<- nrow(df_grafico3)

ggplot(df_grafico3, aes(x = Ano, y = b & c )) +
  geom_bar(stat = "identity", aes(y = b, fill = "Moedas/Depósitos" ), position = position_nudge(x = -.15), width = .3)+
  geom_bar(stat = "identity", aes(y = c, fill = "Imóveis"  ), position = position_nudge(x = .15), width = .3) +
  scale_y_continuous("US$ milhões")+
  scale_x_yearmon(NULL, format = "%Y", n = qanos)+
  scale_fill_manual(NULL, values = saturation(c("#252A52","#FFC465","#BF1B0B"), scalefac(0.8)))+
  theme_classic ()+
  theme(panel.grid = element_blank(), # remove as linhas do corpo do gráfico
        # sem bordas entre os painéis
        panel.spacing = unit(0, "cm"),
        # modifica o texto dos eixos
        axis.text = element_text(size = 12, colour = "black"),
        # cor dos marcadores
        axis.ticks = element_line(colour = "black"),
        # tamanho dos marcadores
        axis.ticks.length = unit(.2, "cm"), 
        #cor da borda
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position=) 

#### IBD - SETOR DE ATIVIDADE ECONOMICA ####

DF_IDE_Por_Setor<- DF_IDE_Por_Setor %>%
  mutate( IDE_Por_Setor = as.numeric(IDE_Por_Setor))

BB <- DF_IDE_Por_Setor %>%
  pivot_wider(names_from = "ANO: 2020", values_from = "IDE_Por_Setor") %>%
  filter(Pais == país)%>%
  group_by("ANO: 2020",Pais ) %>% 
  mutate_all(coalesce, 0)

BB<-BB %>%
  select(Pais, b = "L - Atividades Imobiliárias", c = "G - Comércio, Reparação de Veículos Automotores e Motocicletas",
         d = "N - Atividades Administrativas e Serviços Complementares", e = "C - Indústrias de Transformação",
         f = "K - Atividades Financeiras, de Seguros e Serviços Relacionados", h = "Total país em US$ milhões")%>%
  mutate( "Atividades Imobiliárias" = b*100/h,
          "Comércio, Reparação de Veículos Automotores e Motocicletas" = c*100/h,
          "Atividades Adiministrativas e Serviços Complementares" = d*100/h,
          "Indústrias de Transformação" = e*100/h,
          "Atividades Financeiras, de Seguros e Serviços Relacionados" = f*100/h,
          "Outros" = (h-b-c-d-e-f)*100/h)%>%
  pivot_longer(cols = c("Atividades Imobiliárias":"Outros"), names_to = "Setor", values_to = "value")


BB %>%
  ggplot(aes(x="", y = value, fill= Setor), label = value)+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y", start = 0)+
  theme_void()+
  geom_label_repel(label = paste(round(BB$value,2),"%"), show.legend = F, size = 5, position =  position_stack(vjust = 0.5))

