package entity

import "html/template"

const INDEX_HTML = `
	<html>
		<body>
			<div>
				<table cellpadding="0" cellspacing="0" border="0" width="100%"
				style="background: whitesmoke; min-width: 320px; font-size: 1px; line-height: normal;">
					<tr>
						<td align="center" valign="top">
								<table cellpadding="0" cellspacing="0" border="5" width="700"
							style="background: black; color: whitesmoke; font-family: Arial, Helvetica, sans-serif;">
									<tr>
										<td align="center" valign="top">
											<span style="font-size: 20px; font-weight: bold; color: gold;
										line-height: 40px; -webkit-text-size-adjust:none; display: block;">
												Здравствуйте, {{.Name}} !
											</span>
											<hr width="600" size="1" color="whitesmoke" noshade>
											<span style="font-size: 16px; font-style: italic;
										line-height: 40px; -webkit-text-size-adjust:none; display: block;">
												{{.Message}}
											</span>
										</td>
									</tr>
									<tr>
										<td align="center" valign="top">
											<img src="https://i1.wp.com/divedigital.id/wp-content/uploads/2021/12/48-1.jpg" alt="" width="350" height="350"/>
										</td>
									</tr>
									<tr>
									</tr>
								</table>
						</td>
					</tr>
				</table>
			</div>
		</body>
	</html>`

var IndexHtml = template.Must(template.New("index").Parse(INDEX_HTML))
