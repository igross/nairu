// Presentation only: keep source observations, intervals and model outputs intact.
// Keep release identification outside the Plotly legend so it cannot be clipped
// by a small screen or confused with the cash-rate outcome lines.
function releaseKey(frame, traces) {
  const releases=new Map();
  for(const trace of traces) {
    if(trace.mode!=='lines' || !trace.line?.dash || trace.line.dash==='solid' || trace.x?.length!==2 || trace.x[0]!==trace.x[1]) continue;
    const text=Array.isArray(trace.text)?trace.text[0]:trace.text;
    const match=typeof text==='string' && text.match(/^<b>(CPI|WPI|National Accounts|Labour Force)<\/b>/);
    if(!match) continue;
    const label=match[1], parts=text.split('<br>');
    const date=parts.find(p=>/^\d{1,2} [A-Za-z]{3} \d{4}$/.test(p));
    if(!releases.has(label))releases.set(label,{colour:trace.line.color,dates:new Set()});
    if(date)releases.get(label).dates.add(date);
  }
  if(!releases.size)return;
  let key=frame.nextElementSibling;
  if(!key?.classList.contains('release-key')) {
    key=document.createElement('div');key.className='release-key';key.setAttribute('role','group');key.setAttribute('aria-label','ABS data releases: dashed vertical lines');frame.after(key);
  }
  key.replaceChildren();
  const heading=document.createElement('strong');heading.textContent='ABS releases · dashed vertical lines';key.append(heading);
  for(const [label,{colour,dates}] of releases) {
    const item=document.createElement('span'),swatch=document.createElement('i'),name=document.createElement('span');
    swatch.style.borderColor=colour;swatch.setAttribute('aria-hidden','true');
    name.textContent=label==='National Accounts'?'National Accounts (GDP)':label;
    if(dates.size){const small=document.createElement('small');small.textContent=[...dates].join(' · ');name.append(small);}
    item.append(swatch,name);key.append(item);
  }
}

const mobileBlocks=new WeakMap();
const plainText=value=>String((typeof value==='object'?value?.text:value)||'').replace(/<br\s*\/?\s*>/gi,' ').replace(/<[^>]*>/g,'').replaceAll('&amp;','&').replaceAll('&nbsp;',' ');
function mobileChartText(frame,plot,source,small,win){
  let blocks=mobileBlocks.get(frame);
  if(!blocks){
    const heading=document.createElement('div'),footer=document.createElement('div');
    heading.className='mobile-chart-heading';footer.className='mobile-chart-footer';
    frame.before(heading);frame.parentElement.append(footer);
    blocks={heading,footer};mobileBlocks.set(frame,blocks);
  }
  blocks.heading.hidden=blocks.footer.hidden=!small;
  if(!small)return;
  blocks.heading.replaceChildren();blocks.footer.replaceChildren();
  const title=document.createElement('strong');title.textContent=plainText(source.title?.text||source.title);blocks.heading.append(title);
  const units=new Set(Object.entries(source).filter(([k])=>/^yaxis\d*$/.test(k)).map(([,a])=>plainText(a.title?.text||a.title)).filter(Boolean));
  for(const a of source.annotations||[])if(a.annotationType==='axis'&&a.textangle===-90)units.add(plainText(a.text));
  if(units.size){const unit=document.createElement('small');unit.textContent=[...units].join(' · ');blocks.heading.append(unit);}
  if(source.showlegend!==false || /nairu_history/.test(frame.getAttribute('src'))){
    const legend=document.createElement('div');legend.className='mobile-chart-legend';legend.setAttribute('aria-label','Chart series');
    const seen=new Set();
    plot.data.forEach((trace,index)=>{
      if(!trace.name||trace.showlegend===false||seen.has(trace.name))return;seen.add(trace.name);
      const button=document.createElement('button'),swatch=document.createElement('i'),label=document.createElement('span');button.type='button';
      button.setAttribute('aria-pressed',String(trace.visible!=='legendonly'));button.title='Show or hide '+plainText(trace.name);
      swatch.style.borderColor=typeof trace.line?.color==='string'?trace.line.color:typeof trace.marker?.color==='string'?trace.marker.color:'#536575';
      if(trace.line?.dash&&trace.line.dash!=='solid')swatch.style.borderTopStyle='dashed';
      label.textContent=plainText(trace.name);button.append(swatch,label);
      button.addEventListener('click',()=>{
        const show=button.getAttribute('aria-pressed')!=='true';
        const indices=plot.data.map((t,i)=>trace.legendgroup?t.legendgroup===trace.legendgroup?i:-1:t.name===trace.name?i:-1).filter(i=>i>=0);
        win.Plotly.restyle(plot,{visible:show?true:'legendonly'},indices.length?indices:[index]);button.setAttribute('aria-pressed',String(show));
      });legend.append(button);
    });
    if(legend.childElementCount)blocks.footer.append(legend);
  }
  for(const a of source.annotations||[])if(a.yref==='paper'&&a.y<0&&a.annotationType!=='axis'){
    const note=document.createElement('p');note.textContent=plainText(a.text);blocks.footer.append(note);
  }
}

const originals = new WeakMap();
async function styleFrame(frame) {
  try {
    const doc=frame.contentDocument, win=frame.contentWindow;
    if(!doc || !win.Plotly) return;
    if(!doc.getElementById('dashboard-chart-style')) {
      const style=doc.createElement('style'); style.id='dashboard-chart-style';
      style.textContent='body{margin:0!important;background:white!important;font-family:Arial,Helvetica,sans-serif!important}.modebar{opacity:.35;transition:opacity .2s}.modebar:hover{opacity:1}@media(max-width:540px){.modebar{display:none!important}}';
      doc.head.append(style);
    }
    for(const plot of doc.querySelectorAll('.js-plotly-plot')) {
      if(!plot.layout || !plot.data || plot.dataset.styling || plot.dataset.styledWidth===String(frame.clientWidth)) continue;
      releaseKey(frame,plot.data);
      plot.dataset.styling='true';
      try {
        if(!originals.has(plot)) originals.set(plot, JSON.parse(JSON.stringify(plot.layout)));
        const source=originals.get(plot), width=frame.clientWidth, small=width<540;
        const patch={'font':{family:'Arial, Helvetica, sans-serif',color:'#142f45',size:12},paper_bgcolor:'#fff',plot_bgcolor:'#fff',
          'margin.l':small?38:66,'margin.r':small?20:40,'margin.t':small?25:70,'margin.b':small?45:110,
          'title.text':small?'':source.title?.text||source.title||'',
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
            [key+'.title.text']:small?'':plainText(axis.title),[key+'.title.font']:{family:'Arial',size:12,color:'#536575'},[key+'.automargin']:true,
            [key+'.gridcolor']:'#e7ecef',[key+'.gridwidth']:1,[key+'.showgrid']:!x,[key+'.zerolinecolor']:'#b9c5ce'});
          if(x && axis.tickvals && axis.ticktext) {
            const step=Math.max(1,Math.ceil(axis.tickvals.length/(small?4:9)));
            patch[key+'.tickvals']=axis.tickvals.filter((_,i)=>i%step===0);
            patch[key+'.ticktext']=axis.ticktext.filter((_,i)=>i%step===0);
          } else if(x && axis.type==='date') {patch[key+'.tickmode']='auto';patch[key+'.nticks']=small?4:9;}
        }
        if(source.annotations) patch.annotations=source.annotations.filter(a=>!small || !(a.annotationType==='axis'||(a.yref==='paper'&&a.y<0))).map(a=>{
          const out={...a,font:{...a.font,family:'Arial',size:small?9:11,color:'#536575'}};
          if(a.yref==='paper' && a.y<0) {out.y=frame.getAttribute('src').includes('cash_rate_forecast_paths')?-.17:-.3;out.x=0;out.xanchor='left';
            if(small && typeof a.text==='string') out.text=a.text.replace(/(.{1,46})(?:\s|$)/g,'$1<br>');}
          return out;
        });
        // Match the ggplot panel outline to the axes, keeping event annotations.
        if(source.shapes) patch.shapes=source.shapes.map(s=>s.type==='rect'&&s.xref==='paper'&&s.yref==='paper'?{...s,line:{...s.line,color:'#b9c5ce',width:1}}:s);
        const primary=/nairu_history/.test(frame.getAttribute('src'));
        patch.showlegend=small?false:(primary?true:source.showlegend??true);
        const history=/nairu_(history|model_average)/.test(frame.getAttribute('src'));
        for(let i=0;i<plot.data.length;i++) {
          const trace=plot.data[i], update={};
          if(trace.type==='heatmap') Object.assign(update,{colorscale:[[[0,'#f4f8fb'],[.25,'#c7dce9'],[.5,'#82b2ce'],[.75,'#397fa9'],[1,'#142f45']]], 'colorbar.thickness':small?10:14,'colorbar.tickfont.size':10,'colorbar.title.font.size':11,'colorbar.title.side':small?'right':'top'});
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
        if(small&&plot.data.some(t=>t.type==='heatmap'))patch['margin.r']=65;
        mobileChartText(frame,plot,source,small,win);
        await win.Plotly.relayout(plot,patch);
        await win.Plotly.Plots.resize(plot);
        plot.dataset.siteStyled='true';
        plot.dataset.styledWidth=String(width);
      } finally {delete plot.dataset.styling;}
    }
  } catch(e) {console.warn('Chart styling unavailable',e);}
}
for(const frame of document.querySelectorAll('iframe')) {
  frame.addEventListener('load',()=>{styleFrame(frame);for(const delay of [500,1500,3000,6000,12000,20000]) setTimeout(()=>styleFrame(frame),delay);});
  styleFrame(frame);
  new ResizeObserver(()=>setTimeout(()=>styleFrame(frame),250)).observe(frame);
}
for(const button of document.querySelectorAll('.tab-button')) button.addEventListener('click',()=>requestAnimationFrame(()=>document.querySelectorAll('.tab-content.active iframe').forEach(styleFrame)));
let resizeTimer;
window.addEventListener('resize',()=>{clearTimeout(resizeTimer);resizeTimer=setTimeout(()=>document.querySelectorAll('iframe').forEach(styleFrame),200);});
