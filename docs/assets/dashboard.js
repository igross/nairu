// Style embedded interactive charts without changing their traces or data.
function styleFrame(frame){
  try{
    const doc=frame.contentDocument,win=frame.contentWindow;if(!doc)return;
    if(!doc.getElementById('dashboard-chart-style')){const s=doc.createElement('style');s.id='dashboard-chart-style';s.textContent='body{margin:0!important;background:#fff!important;font-family:Arial,Helvetica,sans-serif!important}.js-plotly-plot .plotly text{font-family:Arial,Helvetica,sans-serif!important}';doc.head.append(s);}
    const plots=[...doc.querySelectorAll('.js-plotly-plot')];
    for(const plot of plots){if(win.Plotly&&plot.layout&&!plot.dataset.siteStyled){plot.dataset.siteStyled='true';win.Plotly.relayout(plot,{'font.family':'Arial, Helvetica, sans-serif','font.color':'#142f45','paper_bgcolor':'#ffffff'});}}
    if(win.Plotly)plots.forEach(p=>{if(p.layout)win.Plotly.Plots.resize(p);});
  }catch(e){/* Standalone external charts retain their own presentation. */}
}
for(const frame of document.querySelectorAll('iframe')){frame.addEventListener('load',()=>{styleFrame(frame);setTimeout(()=>styleFrame(frame),500);});styleFrame(frame);}
for(const button of document.querySelectorAll('.tab-button'))button.addEventListener('click',()=>requestAnimationFrame(()=>document.querySelectorAll('.tab-content.active iframe').forEach(styleFrame)));
