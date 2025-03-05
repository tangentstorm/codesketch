mod ui;
mod app;

use anyhow::Result;
use crate::types::Root;

pub use app::App;

/// Run the interactive browser with the provided code definitions
pub fn run_browser(root: Root) -> Result<()> {
    // Setup terminal
    use crossterm::{
        event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind},
        execute,
        terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    };
    use std::{io, time::{Duration, Instant}};
    use ratatui::{backend::CrosstermBackend, Terminal};

    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create app state
    let mut app = App::new(root);
    let tick_rate = Duration::from_millis(100);

    // Main loop
    let mut last_tick = Instant::now();
    loop {
        // Draw UI
        terminal.draw(|f| ui::draw_ui(f, &mut app))?;

        // Handle events
        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));

        if crossterm::event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    if app.search_mode {
                        match key.code {
                            KeyCode::Esc => app.exit_search(),
                            KeyCode::Backspace => app.remove_search_char(),
                            KeyCode::Enter => {
                                app.exit_search();
                                if !app.search_results.is_empty() {
                                    app.next_search_result();
                                }
                            },
                            KeyCode::Down | KeyCode::Char('n') => app.next_search_result(),
                            KeyCode::Up | KeyCode::Char('p') => app.prev_search_result(),
                            KeyCode::Char(c) => app.add_search_char(c),
                            _ => {}
                        }
                    } else {
                        match key.code {
                            KeyCode::Char('q') => break,
                            KeyCode::Char('/') => app.enter_search(),
                            KeyCode::Down => app.next(),
                            KeyCode::Up => app.previous(),
                            KeyCode::Tab => app.toggle_collapsed(),
                            KeyCode::Enter => app.select(),
                            KeyCode::Left => app.back(),
                            KeyCode::Right => app.forward(),
                            _ => {}
                        }
                    }
                }
            }
        }

        if last_tick.elapsed() >= tick_rate {
            app.tick();
            last_tick = Instant::now();
        }
    }

    // Restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    Ok(())
}