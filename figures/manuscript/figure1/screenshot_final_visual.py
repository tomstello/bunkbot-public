"""Render final_visual.html to figure1_transcript.png (the manuscript Figure 1).
Paths resolve relative to this script, so it can be run from any working directory."""
import asyncio
from pathlib import Path
from playwright.async_api import async_playwright

SCRIPT_DIR = Path(__file__).resolve().parent
HTML_FILE = SCRIPT_DIR / "final_visual.html"
OUTPUT = SCRIPT_DIR / "figure1_transcript.png"


async def main():
    async with async_playwright() as p:
        browser = await p.chromium.launch()
        context = await browser.new_context(
            device_scale_factor=3.0, viewport={"width": 1192, "height": 300}
        )
        page = await context.new_page()
        file_url = HTML_FILE.as_uri()
        print(f"Loading {file_url}")
        await page.goto(file_url)
        try:
            await page.wait_for_load_state("networkidle", timeout=5000)
        except Exception:
            print("Network idle timeout; continuing")
        await page.wait_for_timeout(1000)
        await page.screenshot(path=str(OUTPUT), full_page=True)
        print(f"Saved {OUTPUT}")
        await browser.close()


if __name__ == "__main__":
    asyncio.run(main())
