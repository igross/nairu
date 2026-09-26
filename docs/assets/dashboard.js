// Presentation only: keep source observations, intervals and model outputs intact.
const originals = new WeakMap();
async function styleFrame(frame) {
  try {
    const doc=frame.contentDocument, win=frame.contentWindow;
    if(!doc || !win.Plotly) return;
    if(!doc.getElementById('dashboard-chart-style')) {
      const style=doc.createElement('style'); style.id='dashboard-chart-style';
      style.textContent='body{margin:0!important;background:white!important;font-family:Arial,Helvetica,sans-serif!important}.modebar{opacity:.35;transition:opacity .2s}.modebar:hover{opacity:1}';
      doc.head.append(style);
    }
    for(const plot of doc.querySelectorAll('.js-plotly-plot')) {
      if(!plot.layout || !plot.data || plot.dataset.styling || plot.dataset.styledWidth===String(frame.clientWidth)) continue;
      plot.dataset.styling='true';
      try {
        if(!originals.has(plot)) originals.set(plot, JSON.parse(JSON.stringify(plot.layout)));
        const source=originals.get(plot), small=frame.clientWidth<540;
        const patch={'font':{family:'Arial, Helvetica, sans-serif',color:'#142f45',size:12},paper_bgcolor:'#fff',plot_bgcolor:'#fff',
          'margin.l':small?48:66,'margin.r':small?24:40,'margin.t':70,'margin.b':small?145:110,
          'title.font':{family:'Arial, Helvetica, sans-serif',size:small?12:17,color:'#142f45'},'title.x':0,'title.xanchor':'left',
          'legend.orientation':'h','legend.x':0,'legend.xanchor':'left','legend.y':-.2,'legend.yanchor':'top',
          'legend.font':{size:small?10:12,color:'#536575'},'legend.bgcolor':'rgba(255,255,255,0)','legend.borderwidth':0,
          'hoverlabel':{bgcolor:'#142f45',bordercolor:'#142f45',font:{family:'Arial',color:'#fff',size:12}}};
        for(const [key,axis] of Object.entries(source)) {
          if(!/^[xy]axis\d*$/.test(key)) continue;
          const x=key.startsWith('x');
          Object.assign(patch,{[key+'.showline']:true,[key+'.mirror']:true,[key+'.linecolor']:'#b9c5ce',[key+'.linewidth']:1,
            [key+'.ticks']:'outside',[key+'.ticklen']:4,[key+'.tickwidth']:1,[key+'.tickcolor']:'#9baab5',
            [key+'.tickfont']:{family:'Arial',size:small?10:12,color:'#536575'},[key+'.tickangle']:0,
            [key+'.title.font']:{family:'Arial',size:12,color:'#536575'},[key+'.automargin']:true,
            [key+'.gridcolor']:'#e7ecef',[key+'.gridwidth']:1,[key+'.showgrid']:!x,[key+'.zerolinecolor']:'#b9c5ce'});
          if(x && axis.tickvals && axis.ticktext) {
            const step=Math.max(1,Math.ceil(axis.tickvals.length/(small?4:9)));
            patch[key+'.tickvals']=axis.tickvals.filter((_,i)=>i%step===0);
            patch[key+'.ticktext']=axis.ticktext.filter((_,i)=>i%step===0);
          } else if(x && axis.type==='date') {patch[key+'.tickmode']='auto';patch[key+'.nticks']=small?4:9;}
        }
        if(source.annotations) patch.annotations=source.annotations.map(a=>{
          const out={...a,font:{...a.font,family:'Arial',size:small?9:11,color:'#536575'}};
          if(a.yref==='paper' && a.y<0) {out.y=-.3;out.x=0;out.xanchor='left';
            if(small && typeof a.text==='string') out.text=a.text.replace(/(.{1,46})(?:\s|$)/g,'$1<br>');}
          return out;
        });
        // Match the ggplot panel outline to the axes, keeping event annotations.
        if(source.shapes) patch.shapes=source.shapes.map(s=>s.type==='rect'&&s.xref==='paper'&&s.yref==='paper'?{...s,line:{...s.line,color:'#b9c5ce',width:1}}:s);
        const primary=/nairu_history/.test(frame.getAttribute('src'));
        if(primary) patch.showlegend=true;
        const history=/nairu_(history|model_average)/.test(frame.getAttribute('src'));
        for(let i=0;i<plot.data.length;i++) {
          const trace=plot.data[i], update={};
          if(trace.type==='heatmap') Object.assign(update,{colorscale:[[[0,'#f4f8fb'],[.25,'#c7dce9'],[.5,'#82b2ce'],[.75,'#397fa9'],[1,'#142f45']]], 'colorbar.thickness':small?10:14,'colorbar.tickfont.size':10,'colorbar.title.font.size':11});
          if(trace.type==='scatter' && trace.mode?.includes('lines') && !trace.fill) update['line.width']=trace.line?.dash && trace.line.dash!=='solid'?1.3:2.2;
          if(primary) {
            if(i===1) Object.assign(update,{name:'NAIRU',showlegend:true});
            if(i===2) Object.assign(update,{name:'Unemployment',showlegend:true});
            if(i===3) Object.assign(update,{'marker.color':'#287863','marker.size':5});
          }
          if(history) {
            if(trace.fill) update.fillcolor='rgba(40,120,99,0.13)';
            const colour=trace.line?.color;
            if(colour==='rgba(255,0,0,1)') update['line.color']='#287863';
            if(colour==='rgba(0,0,255,1)') update['line.color']='#175da0';
          }
          if(trace.name==='No change') update['line.color']='#657583';
          if(trace.name==='-25 bp cut') update['line.color']='#175da0';
          if(trace.name==='+25 bp hike') update['line.color']='#bd554f';
          if(Object.keys(update).length) await win.Plotly.restyle(plot,update,[i]);
        }
        await win.Plotly.relayout(plot,patch);
        await win.Plotly.Plots.resize(plot);
        plot.dataset.siteStyled='true';
        plot.dataset.styledWidth=String(frame.clientWidth);
      } finally {delete plot.dataset.styling;}
    }
  } catch(e) {console.warn('Chart styling unavailable',e);}
}
for(const frame of document.querySelectorAll('iframe')) {
  frame.addEventListener('load',()=>{styleFrame(frame);for(const delay of [500,1500,3000,6000,12000,20000]) setTimeout(()=>styleFrame(frame),delay);});
  styleFrame(frame);
}
for(const button of document.querySelectorAll('.tab-button')) button.addEventListener('click',()=>requestAnimationFrame(()=>document.querySelectorAll('.tab-content.active iframe').forEach(styleFrame)));
let resizeTimer;
window.addEventListener('resize',()=>{clearTimeout(resizeTimer);resizeTimer=setTimeout(()=>document.querySelectorAll('iframe').forEach(styleFrame),200);});
